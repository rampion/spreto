{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module UI where

import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (fix)
import Data.Maybe (isJust, fromMaybe)
import Data.Semigroup ((<>))

import Brick hiding (Direction)
import Brick.BChan (BChan, writeBChan)
import Brick.Widgets.Center (hCenter)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import System.Clock (getTime, Clock(Monotonic), TimeSpec(..))

import ORP (orp)
import Cursor (Cursor, move, Increment(..), getWord)

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings
type Microseconds = Double
type WPM = Double
type FrameIndex = Int

-- | environment parameters given by the user
data Environment = Env
  { introDuration   :: Maybe Microseconds
  , unpauseDuration :: Maybe Microseconds
  , events          :: BChan Event
  , attributes      :: AttrMap
  }

-- | derived environment parameters
data Derived = Der
  { reticuleWidth             :: Int
  , prefixWidth               :: Int
  , suffixWidth               :: Int
  , lastIntroFrame            :: FrameIndex
  , lastUnpauseFrame          :: FrameIndex
  , introFramesPerMicrosecond :: Double
  , reticule :: Int -> [Widget Name] -> Widget Name
  }

data State = St
  { wpm         :: !WPM
  , direction   :: !Direction
  , mode        :: !Mode
  , here        :: !Cursor
  , progress    :: !Progress
  }

data Direction
  = Forwards
  | Backwards
  deriving (Eq)

data Mode 
  = Starting
  | Introing { timer :: !ThreadId, frameNum :: !FrameIndex, startTime :: !Microseconds }
  | Reading { timer :: !ThreadId, lastMove :: !Microseconds }
  | Paused { showHelp :: !Bool }
  | Unpausing { timer :: !ThreadId, frameNum :: !FrameIndex, startTime :: !Microseconds }

data Progress
  = Percentage
  | FractionTime
  | FractionParagraphs
  | FractionSentences
  | FractionWords

newtype Event
  = Advance { source :: ThreadId }

-- | Named Resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

type Command = Environment -> Derived -> State -> EventM Name (Next State)
type Binding = (Vty.Key, Command)

makeApp :: Environment -> App State Event Name
makeApp env = App
  { appDraw         = draw env der
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent env der
  , appStartEvent   = cueStart env der
  , appAttrMap      = const (attributes env)
  }
  where der = derived env

derived :: Environment -> Derived
derived Env{..} = Der {..} where
  prefixWidth = orp (Text.replicate reticuleWidth "X")
  suffixWidth = reticuleWidth - 1 - prefixWidth
  lastIntroFrame = lcm (prefixWidth * numLeftEdges) (suffixWidth * numRightEdges)
  introFramesPerMicrosecond = fromIntegral lastIntroFrame  / fromMaybe 0 introDuration
  lastUnpauseFrame          = undefined

  reticule n xs = vBox
    [ str reticuleTop
    , padLeft (Pad n) (hBox xs)
    , str reticuleBottom
    ]

  reticuleTop     = reticuleLine '┬'
  reticuleBottom  = reticuleLine '┴'
  reticuleLine c = replicate prefixWidth '─' ++ c : replicate suffixWidth '─'

usage :: [(String, [Binding])]
usage = 
  [ ("pause/unpause", [(Vty.KChar ' ', togglePause)]) 
  , ("quit", [(Vty.KChar 'q', quit)])
  , ("pause and toggle the help menu"
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
  , ("go back one word and pause", [(Vty.KLeft, pauseAndStepOneWord Backwards)])
  , ("go forwards one word and pause", [(Vty.KRight, pauseAndStepOneWord Forwards)])
  , ("increase wpm", [(Vty.KUp, increaseWPM)])
  , ("decrease wpm", [(Vty.KDown, decreaseWPM)])
  , ("pause and change progress display", [(Vty.KChar '\t', pauseAndChangeProgress)])
  ]

bindings :: Map Vty.Key Command
bindings = Map.fromList $ snd =<< usage

getCurrentTime :: MonadIO m => m Microseconds
getCurrentTime = liftIO $ do
  TimeSpec{..} <- getTime Monotonic
  return $ 1000*1000*fromIntegral sec + fromIntegral nsec / 1000

draw :: Environment -> Derived -> State -> [Widget Name]
draw Env{..} der@Der{..} St{..} = return $ case mode of
  Starting -> emptyWidget
  Introing{..} ->
    let framesRemaining = lastIntroFrame - frameNum
        (leftFull, leftPartial)   = (prefixWidth * framesRemaining) `quotRem` lastIntroFrame
        (rightFull, rightPartial) = (suffixWidth * framesRemaining) `quotRem` lastIntroFrame
        numFull = leftFull + 1 + rightFull
        leftEdge  = introLeftEdges ! quot (numLeftEdges * leftPartial) lastIntroFrame
        rightEdge = introRightEdges ! quot (numRightEdges * rightPartial) lastIntroFrame
        indent = prefixWidth - leftFull - length leftEdge

    in
    reticule indent 
      [ str leftEdge
      , str $ replicate numFull '█'
      , str rightEdge
      ]
      
  Reading{..} ->
    let word = getWord here
        n = orp word
        (pre,Just (c, suf)) = Text.uncons <$> Text.splitAt n word
    in 
    reticule (prefixWidth - n)
      [ withAttr wordPrefix $ txt pre
      , withAttr wordFocus  $ str [c]
      , withAttr wordSuffix $ txt suf
      ] 

{-
    ────────────────────┬───────────────────────────────────────────────────────────
    …umphantly.  Alice did not quite know what to say to this: so she helped hersel…
    ────────────────────┴───────────────────────────────────────────────────────────
    250wpm                          1:00:17/1:57:51                          403.0.1

    ────────────────────┬───────────────────────────────────────────────────────────
    …umphantly.  Alice did not quite know what to say to this: so she helped hersel…
    ────────────────────┴───────────────────────────────────────────────────────────
    250wpm                                51%                                403.0.1

    ────────────────────┬───────────────────────────────────────────────────────────
    …umphantly.  Alice did not quite know what to say to this: so she helped hersel…
    ────────────────────┴───────────────────────────────────────────────────────────
    250wpm                        403/881 paragraphs                         403.0.1

    ────────────────────┬───────────────────────────────────────────────────────────
    …umphantly.  Alice did not quite know what to say to this: so she helped hersel…
    ────────────────────┴───────────────────────────────────────────────────────────
    250wpm                        1104/2290 sentences                        403.0.1

    ────────────────────┬───────────────────────────────────────────────────────────
    …umphantly.  Alice did not quite know what to say to this: so she helped hersel…
    ────────────────────┴───────────────────────────────────────────────────────────
    250wpm                         15073/29465 words                         403.0.1
-}
  Paused{..} -> 
    let (cpre, wpre, wfoc, wsuf, csuf) = context der here
    in
    vBox
      [ reticule (prefixWidth - Text.length cpre - Text.length wpre)
          [ withAttr contextPrefix $ txt cpre
          , withAttr wordPrefix $ txt wpre
          , withAttr wordFocus $ str [wfoc]
          , withAttr wordSuffix $ txt wsuf
          , withAttr contextSuffix $ txt csuf
          ]
      , hBox
        [ str "left"
        , hCenter $ str "center"
        , str "right"
        ]
      ]

  Unpausing{..} -> undefined
    -- cropLeftBy / cropRightBy

context :: Derived -> Cursor -> (Text, Text, Char, Text, Text)
context Der{..} here = (cpre, wpre, wfoc, wsuf, csuf) where
  word = getWord here
  n = orp word
  (wpre,Just (wfoc, wsuf)) = Text.uncons <$> Text.splitAt n word
  cpreWidth = prefixWidth - Text.length wpre
  csufWidth = suffixWidth - Text.length wsuf
  
  cpre = flip fix ("", move ToPreviousWord here) $ \loop (cpre, moved) -> 
    case (Text.length cpre `compare` cpreWidth, moved) of
      (GT, _)         -> "…" <> Text.takeEnd (cpreWidth - 1) cpre
      (_, Just here)  -> loop (getWord here <> " " <> cpre, move ToPreviousWord here)
      _               -> cpre

  csuf = flip fix ("", move ToNextWord here) $ \loop (csuf, moved) ->
    case (Text.length csuf `compare` csufWidth, moved) of
      (GT, _)         -> Text.take (csufWidth - 1) csuf <> "…"
      (_, Just here)  -> loop (csuf <> " " <> getWord here, move ToNextWord here)
      _               -> csuf

numLeftEdges, numRightEdges :: Int
numLeftEdges = Vector.length introLeftEdges
numRightEdges = Vector.length introRightEdges

introLeftEdges, introRightEdges :: Vector String
introLeftEdges   = Vector.fromList $ "" : map return "▕▐" 
introRightEdges  = Vector.fromList $ "" : map return "▏▎▍▌▋▊▉"

-- Use variables rather than literals to protect against typos
wordPrefix, wordFocus, wordSuffix, contextPrefix, contextSuffix :: AttrName
wordPrefix  = "wordPrefix"
wordFocus   = "wordFocus"
wordSuffix  = "wordSuffix"
contextPrefix = "contextPrefix"
contextSuffix = "contextSuffix"

cueStart :: Environment -> Derived -> State -> EventM Name State 
cueStart env@Env{..} der St{..} = do
  let next | isJust introDuration = introing env der 0
           | otherwise            = reading env wpm
  mode <- next =<< getCurrentTime
  return St{ .. }

handleEvent :: Environment -> Derived -> State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent env der st (VtyEvent (Vty.EvKey k [])) = case Map.lookup k bindings of 
  Just action -> action env der st
  Nothing     -> continue st
handleEvent env@Env{..} der@Der{..} st@St{..} (AppEvent Advance{..}) = case mode of 
  -- check the source of the event against the current timer to 
  -- make sure it's not leftover from a prior state
  Introing {..} | source == timer -> do
    mode <- if frameNum < lastIntroFrame
      then introing env der (frameNum + 1) startTime
      else reading env wpm =<< getCurrentTime
    continue St{..}
  Unpausing {..} | source == timer -> do
    mode <- if frameNum < lastUnpauseFrame
      then unpausing (frameNum + 1) startTime
      else reading env wpm =<< getCurrentTime
    continue St{..}
  Reading {..} | source == timer ->
    let step = case direction of 
          Forwards  -> ToNextWord
          Backwards -> ToPreviousWord
    in case move step here of 
      Nothing -> halt st
      Just here -> do
        mode <- reading env wpm =<< getCurrentTime
        continue St{..}
  _ -> continue st
handleEvent _env _der st _ev = continue st

introing :: MonadIO m => Environment -> Derived -> FrameIndex -> Microseconds -> m Mode
introing Env{..} Der{..} frameNum startTime  = liftIO $ do
  -- threadDelay only guarantees a minimum delay, other factors
  -- may cause the thread to wait longer.
  --
  -- To avoid having the intro run beyond the desired introDuration, 
  -- find out when the next frame *should* start, and only wait until then
  let nextTime = startTime + (fromIntegral frameNum / introFramesPerMicrosecond)
  timer <- forkIO $ do
    currTime <- getCurrentTime
    threadDelay $ round (nextTime - currTime)
    writeBChan events . Advance =<< myThreadId
  return Introing { .. }

reading :: MonadIO m => Environment -> WPM -> Microseconds -> m Mode
reading Env{..} wpm lastMove = do
  timer <- liftIO . forkIO $ do
    threadDelay $ round (60e6 / wpm)
    writeBChan events . Advance =<< myThreadId
  return Reading { .. }

unpausing :: MonadIO m => FrameIndex -> Microseconds -> m Mode
unpausing = undefined

togglePause :: Command
togglePause _ _ St{..} = case mode of
  Paused _ -> do
    mode <- unpausing 0 =<< getCurrentTime
    continue St{..}
  _ -> continue St{ mode = Paused False, .. }

quit :: Command
quit _ _ = halt

pauseAndToggleHelp :: Command
pauseAndToggleHelp _ _ St{..} = continue St
  { mode = case mode of
      Paused True -> Paused False
      _           -> Paused True
  , ..
  }

chain :: Monad m => Int -> (a -> m a) -> a -> m a
chain n = foldr (>=>) return . replicate n

jumpFiveSeconds :: Direction -> Command
jumpFiveSeconds dir _env _der st@St{..} = 
  let step = if dir == direction
        then ToNextWord
        else ToPreviousWord
      numSteps = min 1 $ round (wpm/12)
  in case chain numSteps (move step) here of
    Nothing -> halt st
    Just here -> continue st { here = here }

jumpOneSentence :: Direction -> Command
jumpOneSentence dir _env _der st@St{..} = 
  let step = if dir == direction
        then ToNextSentence
        else ToPreviousSentence 
  in case move step here of
    Nothing -> halt st
    Just here -> continue st { here = here }

jumpOneParagraph :: Direction -> Command
jumpOneParagraph dir _env _der st@St{..} = 
  let step = if dir == direction
        then ToNextParagraph
        else ToPreviousParagraph 
  in case move step here of
    Nothing -> halt st
    Just here -> continue st { here = here }

toggleReadingDirection :: Command
toggleReadingDirection _ _ St{..} = continue St
  { direction = case direction of 
      Forwards -> Backwards
      Backwards -> Forwards
  , ..
  }

pauseAndStepOneWord :: Direction -> Command
pauseAndStepOneWord dir _env _der St{..} = continue St
  { mode = case mode of
      Paused _  -> mode 
      _         -> Paused False
    -- don't quit if user tries to step past end, just noop
  , here = fromMaybe here (move step here)
  , ..
  }
  where step = if dir == direction
          then ToNextWord
          else ToPreviousWord

increaseWPM :: Command
increaseWPM _env _der st@St{..} = continue st { wpm = wpm + 1 }

decreaseWPM :: Command
decreaseWPM _env _der st@St{..} = continue st { wpm = wpm - 1 }

pauseAndChangeProgress :: Command
pauseAndChangeProgress _env _der st@St{..} = continue st
  { mode = case mode of
      Paused _  -> mode
      _         -> Paused False
  , progress = case progress of
      Percentage          -> FractionTime
      FractionTime        -> FractionParagraphs
      FractionParagraphs  -> FractionSentences
      FractionSentences   -> FractionWords
      FractionWords       -> Percentage
  }
