module UI where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector

import Brick
import System.Clock (getTime, Monotonic, TimeSpec(..))

import ORP (orp)

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings
type Microseconds = Double

data Env = Env
  { introDuration   :: Maybe Microseconds
  , unpauseDuration :: Maybe Microseconds
  , reticuleWidth   :: Int
  , events          :: BChan Event
  , attributes      :: AttrMap
  }

data Derived = Derived
  { prefixWidth               :: Int
  , suffixWidth               :: Int
  , lastIntroFrame            :: Int
  , introFramesPerMicrosecond :: Double
  }

data State = State
  { wpm         :: !Double
  , direction   :: !Direction
  , mode        :: !Mode
  , here        :: !Cursor
  }

data Direction
  = Forwards
  | Backwards

data Mode 
  = Starting
  | Introing { timer :: !ThreadId, frameNum :: Int, startTime :: !Microseconds }
  | Reading { timer :: !ThreadId, lastMove :: !Microseconds }
  | Paused { showHelp :: !Bool }
  | Unpausing { timer :: !ThreadId, frameNum :: Int, startTime :: !Microseconds }

data Event
  = Advance

-- | Named Resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

type Command = Env -> Derived -> State -> EventM Name (Next State)
type Binding = (Vty.Key, Command)

makeApp :: PartialEnv -> App State Event Name
makeApp env = App
  { appDraw         = draw env
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent env
  , appStartEvent   = return
  , appAttrMap      = const (attributes env)
  }

usage :: [(String, [Binding])]
usage = 
  [ ("pause/unpause", [(Vty.KChar ' ', togglePause)]) 
  , ("quit", [(Vty.KChar 'q', quit)])
  , ("pause and toggle the help menu",
    , [ (Vty.KChar 'h', pauseAndToggleHelp)
      , (Vty.KChar '?', pauseAndToggleHelp)
      ]
    )
  , ("jump back/forward 5 seconds"
    , [ (Vty.KChar 'b', jumpFiveSeconds Backwards)
      , (Vty.KChar 'B', jumpFiveSeconds Forwards)
      ]
    )
  , ("jump back/forward one sentence"
    , [ (Vty.KChar 's', jumpOneSentence Backwards)
      , (Vty.KChar 'S', jumpOneSentence Forwards)
      ]
    )
  , ("jump back/forward one paragraph"
    , [ (Vty.KChar 'p', jumpOneParagraph Backwards)
      , (Vty.KChar 'P', jumpOneParagraph Forwards)
      ]
    )
  , ("toggle reading direction backwards/forwards"
    , [(Vty.KChar 'r', toggleReadingDirection)]
    )
  , ("go back one word and pause", [(Vty.KLeft, pauseAndStep Backwards)])
  , ("go forewards one word and pause", [(Vty.KLeft, pauseAndStep Forewards)])
  , ("increase wpm", [(Vty.KUp, increaseWPM)])
  , ("decrease wpm", [(Vty.KDown, decreaseWPM)])
  ]

bindings :: Map Vty.Key Command
bindings = Map.fromList $ snd =<< usage

getCurrentTime :: IO Microseconds
getCurrentTime = do
  TimeSpec{..} <- getTime Monotonic
  return $ 1000*1000*fromIntegral sec + fromIntegral nsec / 1000

draw :: DerivedEnv -> State -> [Widget Name]
draw Env{..} = return . drawState where

  drawState State{ mode=Introing{..} } =
    let framesRemaining = lastFrame - frameNum
        (leftFull, leftPartial)   = (prefixWidth * framesRemaining) `quotRem` lastFrame
        (rightFull, rightPartial) = (suffixWidth * framesRemaining) `quotRem` lastFrame
        numFull = leftFull + 1 + rightFull
        leftEdge  = introLeftEdges ! quot (numLeftEdges * leftPartial) lastFrame
        rightEdge = introRightEdges ! quot (numRightEdges * rightPartial) lastFrame
        indent = prefixWidth - leftFull - length leftEdge

    in
    reticule indent 
      [ str leftEdge
      , str $ replicate numFull '█'
      , str rightEdge
      ]
      
  drawState State{ mode=Reading{..}, ..} = 
    let word = getWord here
        n = orp word
        (pre,Just (c, suf)) = Text.uncons <$> Text.splitAt n word
    in 
    reticule (prefixWidth - n)
      [ withAttr wordPrefix $ txt pre
      , .withAttr wordFocus  $ str [c]
      , withAttr wordSuffix $ txt suf
      ] 

  drawState State{ mode=Paused{..} } = str "Paused"
  drawState State{ mode=Unpausing{..} } = str "Unpausing"
  drawState State{ mode=Starting } = emptyWidget

  reticule n xs = vBox
    [ str reticuleTop
    , padLeft (Pad n) (hBox xs)
    , str reticuleBottom
    ]

  reticuleTop     = reticuleLine '┬'
  reticuleBottom  = reticuleLine '┴'
  reticuleLine c = replicate prefixWidth '─' ++ c : replicate suffixWidth '─'

  prefixWidth = orp (Text.replicate reticuleWidth "X")
  suffixWidth = reticuleWidth - 1 - prefixWidth

  lastFrame = lcm (prefixWidth * numLeftEdges)
                  (suffixWidth * numRightEdges)

numLeftEdges, numRightEdges :: Int
numLeftEdges = Vector.length introLeftEdges
numRightEdges = Vector.length introRightEdges

introLeftEdges, introRightEdges :: Vector String
introLeftEdges   = Vector.fromList $ "" : map return "▕▐" 
introRightEdges  = Vector.fromList $ "" : map return "▏▎▍▌▋▊▉"

-- Use variables rather than literals to protect against typos
wordPrefix, wordFocus, wordSuffix :: AttrName
wordPrefix  = "wordPrefix"
wordFocus   = "wordFocus"
wordSuffix  = "wordSuffix"

handleEvent :: DerivedEnv -> State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent env st (VtyEvent (Vty.EvKey k [])) = case Map.lookup k bindings of 
  Just action -> action env st
  Nothing     -> continue st
handleEvent env@Env{..} st@State{..} (AppEvent Advance) = case mode of 
  Starting -> do
    let next | showIntro = introing 0
             | otherwise = reading wpm
    mode <- next =<< getCurrentTime
    continue State { .. }
  Introing {..} -> do
    let next | frameNum < lastFrame = introing (frameNum + 1)
             | otherwise            = reading wpm
    mode <- next =<< getCurrentTime
    continue State {..}
  Unpausing {..} -> do
    let next | frameNum < lastFrame = unpausing (frameNum + 1)
             | otherwise            = reading wpm
    mode <- next =<< getCurrentTime
    continue State {..}
  Reading {..} ->
    let step = case direction of 
      Forwards  -> ToNextWord
      Backwards -> ToPreviousWord
    in case move step here of 
      Nothing -> halt st
      Just here -> do
        mode <- reading wpm =<< getCurrentTime
        continue State{..}
  Paused {..} -> continue st
handleEvent _env st _ev = continue st

{-

  -- VtyEvent (Vty.EvKey k []) -> case k of

  introing frameNum startTime = do
    let nextTime = startTime + (fromIntegral frameNum / framesPerMicrosecond)
    timer <- forkIO $ do
      currTime <- getCurrentTime
      threadDelay $ round (nextTime - currTime)
      writeBChan events Advance
    return Introing { .. }

  reading wpm lastMove = do
    timer <- liftIO . forkIO $ do
      threadDelay $ round (60e6 / wpm)
      writeBChan events Advance
    return Reading { .. }



  handleStateEvent st@State { mode=Starting, .. } = \case
    AppEvent Advance 
    _ -> continue st

  handleStateEvent st@State{ mode=Introing{..}, ..} = \case
    AppEvent Advance -> do
    _ -> continue st

  handleStateEvent st@State{ mode=Reading{..}, ..} = \case
    AppEvent Advance ->

          
    _ -> continue st

  handleStateEvent st = \_ -> halt st

  -}
  {-
  prefixWidth = orp (Text.replicate reticuleWidth "X")
  suffixWidth = reticuleWidth - 1 - prefixWidth

  lastFrame = lcm (prefixWidth * numLeftEdges)
                  (suffixWidth * numRightEdges)

  introDurationInMicroseconds = 5e6
  framesPerMicrosecond = fromIntegral lastFrame  / introDurationInMicroseconds
-}

clearTimers :: MonadIO m => Mode -> m ()
clearTimers = undefined

togglePause :: Command
togglePause _ _ State{..} = case mode
  Paused _ -> do
    mode <- unpausing 0 =<< getCurrentTime
    continue State {..}
  _ -> continue { mode = Paused False, .. }

quit :: Command
quit _ _ = halt

pauseAndToggleHelp :: Command
pauseAndToggleHelp _ _ State{..} = continue State
  { mode = case mode of
      Paused True -> Paused False
      _           -> Paused True
  , ..
  }

jumpFiveSeconds :: Direction -> Command
jumpFiveSeconds = undefined

jumpOneSentence :: Direction -> Command
jumpOneSentence = undefined

jumpOneParagraph :: Direction -> Command
jumpOneParagraph = undefined

toggleReadingDirection :: Command
toggleReadingDirection _ _ State{..} = continue State
  { direction = case direction of 
      Forwards -> Backwards
      Backwards -> Forwards
  , ..
  }

pauseAndStep :: Direction -> Command
pauseAndStep State{..} step = continue State
  { mode = case mode of
      Paused _  -> mode 
      _         -> Paused False
    -- don't quit if user tries to step past end, just noop
  , here = fromMaybe here (move step here)
  , ..
  }

increaseWPM :: Command
increaseWPM = undefined

decreaseWPM :: Command
decreaseWPM = undefined
