{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module UI where

import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (state, runState, MonadState)
import Data.Function (fix)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intercalate)
import Data.Semigroup ((<>))

import Brick hiding (Direction)
import Brick.BChan (BChan, writeBChan, newBChan)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (textWidth)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import Graphics.Text.Width (wcswidth, wcwidth)
import System.Clock (getTime, Clock(Monotonic), TimeSpec(..))

import ORP (orp, getWordORP, splitWidth, rsplitWidth)
import Cursor (Cursor, move, Increment(..), getWord, getPosition, getDocument)
import Position (Position(..))

type Microseconds = Double
type WPM = Double
type FrameIndex = Int
type Columns = Int

data Configuration = Config
  { uiIntroDuration     :: !(Maybe Microseconds)
  , uiUnpauseDuration   :: !(Maybe Microseconds)
  , uiAttributes        :: !AttrMap
  , uiWPM               :: !WPM
  , uiReadingDirection  :: !Direction
  , uiProgressDisplay   :: !ProgressDisplay
  }

data Environment = Env
  { introDuration     :: !(Maybe Microseconds)
  , unpauseDuration   :: !(Maybe Microseconds)
  , attributes        :: !AttrMap
  , events            :: !(BChan Event)
  , absNumParagraphs  :: !Int
  , absNumSentences   :: !Int
  , absNumWords       :: !Int
  , sentenceOffset    :: !(Vector Int)
  , wordOffset        :: !(Vector (Vector Int))
  }

data State = St
  { wpm             :: !WPM             -- ^ rate at which to show words
  , direction       :: !Direction       -- ^ reading forwards or backwards
  , progressDisplay :: !ProgressDisplay -- ^ how to show progress when paused
  , here            :: !Cursor          -- ^ current position in the text
  , reticuleWidth   :: !Columns
  , prefixWidth     :: !Columns
  , suffixWidth     :: !Columns
  , mode            :: !Mode
  }

data Direction
  = Forwards
  | Backwards
  deriving (Eq)

data ProgressDisplay
  = Percentage
  | FractionTime
  | FractionParagraphs
  | FractionSentences
  | FractionWords
  deriving Enum

data Mode 
  = Starting
  | Introing
    { timer                 :: !ThreadId
    , frameNum              :: !FrameIndex
    , startTime             :: !Microseconds
    , framesPerMicrosecond  :: !Double
    , lastFrame             :: !FrameIndex
    }
  | Reading
    { timer     :: !ThreadId
    , lastMove  :: !Microseconds
    }
  | Paused
    { showHelp  :: !Bool
    }
  | Unpausing
    { timer                 :: !ThreadId
    , frameNum              :: !FrameIndex
    , startTime             :: !Microseconds
    , framesPerMicrosecond  :: !Double
    , lastFrame             :: !FrameIndex
    }

newtype Event
  = Advance { source :: ThreadId }

data Name = Name deriving (Eq, Ord)

type Command = Environment -> State -> EventM Name (Next State)
type Binding = (Vty.Key, Command)

offsets :: MonadState Int m => Vector Int -> m (Vector Int)
offsets = traverse $ \a -> state $ \s -> (s, a+s)

runUI :: Cursor -> Configuration -> IO State
runUI here Config{..} = do
  events <- newBChan 10 -- Q: why 10? A: stolen from the example

  vty <- Vty.mkVty Vty.defaultConfig
  (reticuleWidth, _numRows) <- Vty.displayBounds $ Vty.outputIface vty
      
  let doc = getDocument here
      (wordOffset, absNumWords) = traverse offsets (fmap Vector.length <$> doc) `runState` 0
      (sentenceOffset, absNumSentences) = offsets (Vector.length <$> doc) `runState` 0
      absNumParagraphs = Vector.length doc

      prefixWidth = orp (Text.replicate reticuleWidth "X")
      suffixWidth = reticuleWidth - 1 - prefixWidth

      env = Env
        { introDuration   = uiIntroDuration
        , unpauseDuration = uiUnpauseDuration
        , events          = events
        , attributes      = uiAttributes
        , ..
        }

  -- XXX: takes over the entire display, which is non-optimal
  --      see https://github.com/jtdaugherty/vty/issues/143
  Brick.customMain (return vty) (Just events)
    App { appDraw         = draw env
        , appChooseCursor = neverShowCursor
        , appHandleEvent  = handleEvent env
        , appStartEvent   = cueStart env
        , appAttrMap      = const (attributes env)
        }
    St  { wpm             = uiWPM
        , direction       = uiReadingDirection
        , progressDisplay = uiProgressDisplay
        , mode            = Starting
        , ..
        }

drawReticule :: State -> Columns -> [Widget Name] -> Widget Name
drawReticule St{..} indent xs = vBox
  [ str $ reticuleLine '┬'
  , padLeft (Pad indent) (hBox xs)
  , str $ reticuleLine '┴'
  ]
  where reticuleLine c = replicate prefixWidth '─' ++ c : replicate suffixWidth '─'

bindingsHelp :: [(String, [Binding])]
bindingsHelp = 
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
  , ("pause and change progress display"
    , [ (Vty.KChar '\t', pauseAndChangeProgress Forwards)
      , (Vty.KBackTab, pauseAndChangeProgress Backwards)
      ]
    )
  ]

bindings :: Map Vty.Key Command
bindings = Map.fromList $ snd =<< bindingsHelp

getCurrentTime :: MonadIO m => m Microseconds
getCurrentTime = liftIO $ do
  TimeSpec{..} <- getTime Monotonic
  return $ 1000*1000*fromIntegral sec + fromIntegral nsec / 1000

draw :: Environment -> State -> [Widget Name]
draw env@Env{..} st@St{..} = case mode of
  Starting ->
    return emptyWidget

  Introing{..} ->
    let framesRemaining = lastFrame - frameNum
        (leftFull, leftPartial)   = (prefixWidth * framesRemaining) `quotRem` lastFrame
        (rightFull, rightPartial) = (suffixWidth * framesRemaining) `quotRem` lastFrame
        numFull = leftFull + 1 + rightFull
        leftEdge  = introLeftEdges ! quot (numLeftEdges * leftPartial) lastFrame
        rightEdge = introRightEdges ! quot (numRightEdges * rightPartial) lastFrame
        indent = prefixWidth - leftFull - wcswidth leftEdge
    in
    return $ drawReticule st indent 
      [ str leftEdge
      , str $ replicate numFull '█'
      , str rightEdge
      ]
      
  Reading{..} ->
    let (pre, c, suf) = getWordORP here in 
    return $ drawReticule st (prefixWidth - textWidth pre)
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
    [ let (cpre, wpre, wfoc, wsuf, csuf) = reticuleContext st
      in
      drawReticule st (prefixWidth - textWidth cpre - textWidth wpre)
      [ withAttr contextPrefix $ txt cpre
      , withAttr wordPrefix $ txt wpre
      , withAttr wordFocus $ str [wfoc]
      , withAttr wordSuffix $ txt wsuf
      , withAttr contextSuffix $ txt csuf
      ]

    , translateBy (Location (0,3)) $ hBox
      [ str $ show (round wpm :: Int)
      , str "wpm"
      ]

    , let spos = show $ getPosition here
          rightJustify = reticuleWidth - wcswidth spos
      in
      translateBy (Location (rightJustify,3)) $ str spos

    , padTop (Pad 3) . hCenter . str $ showProgress env st

    , if showHelp
        then padTop (Pad 5) $ hBox $ padLeft (Pad 4) <$>
          [ vBox [ str keys | (_, unzip -> (showKeys -> keys, _)) <- bindingsHelp ]
          , vBox [ str helpMsg | (helpMsg, _) <- bindingsHelp ]
          ]
        else emptyWidget
    ]

  Unpausing{..} -> 
    let (cpre, wpre, wfoc, wsuf, csuf) = reticuleContext st
        framesRemaining = lastFrame - frameNum
        leftFull = ((prefixWidth - textWidth wpre) * framesRemaining) `quot` lastFrame
        rightFull = ((suffixWidth - textWidth wsuf) * framesRemaining) `quot` lastFrame
        cpre' = snd $ rsplitWidth leftFull cpre
        csuf' = fst $ splitWidth rightFull csuf
    in
    [ drawReticule st (prefixWidth - textWidth cpre' - textWidth wpre)
      [ withAttr contextPrefix . txt $ cpre'
      , withAttr wordPrefix $ txt wpre
      , withAttr wordFocus $ str [wfoc]
      , withAttr wordSuffix $ txt wsuf
      , withAttr contextSuffix . txt $ csuf'
      ]
    ]

showProgress :: Environment -> State -> String
showProgress Env{..} St{..} = case progressDisplay of
    FractionParagraphs -> concat
      [ show paragraphNum 
      , "/"
      , show absNumParagraphs
      , " paragraphs"
      ]
    FractionSentences -> concat
      [ show absSentenceNum
      , "/"
      , show absNumSentences
      , " sentences"
      ]
    FractionWords -> concat
      [ show absWordNum
      , "/"
      , show absNumWords
      , " words"
      ]
    Percentage -> concat [ show percentComplete, "%" ]
    FractionTime -> concat [ estTime, "/", totTime ]
  where
    Position{..} = getPosition here

    absWordNum = wordNum + (wordOffset ! paragraphNum ! sentenceNum)
    absSentenceNum = sentenceNum + (sentenceOffset ! paragraphNum)

    percentComplete = round (100 * fromIntegral absWordNum / fromIntegral absNumWords :: Double) :: Int

    estTime = showTime absWordNum
    totTime = showTime absNumWords

    showTime n = concat [ show h , ":", padShow m, ":", padShow s ] where
      hms :: Int
      hms = round $ 60 * fromIntegral n / wpm
      (hm,s) = hms `quotRem` 60
      (h,m)  = hm `quotRem` 60

    padShow n | n < 10 = '0' : show n
              | otherwise = show n

showKeys :: [Vty.Key] -> String
showKeys = intercalate "/" . fmap showKey where
  showKey k = case k of
    Vty.KChar ' '   -> "<Space>"
    Vty.KChar '\t'  -> "<Tab>"
    Vty.KBackTab    -> "<S-Tab>"
    Vty.KUp         -> "<Up>"
    Vty.KDown       -> "<Down>"
    Vty.KLeft       -> "<Left>"
    Vty.KRight      -> "<Right>"
    Vty.KChar c     -> [c]
    _               -> show k

reticuleContext :: State -> (Text, Text, Char, Text, Text)
reticuleContext St{..} = (cpre, wpre, wfoc, wsuf, csuf) where
  (wpre, wfoc, wsuf) = getWordORP here
  cpreWidth = prefixWidth - textWidth wpre
  csufWidth = suffixWidth - textWidth wsuf + 1 - wcwidth wfoc
  ellipsis = "…"
  
  cpre = flip fix ("", move ToPreviousWord here) $ \loop (cpre, moved) -> 
    case (textWidth cpre `compare` cpreWidth, moved) of
      (GT, _)         -> ellipsis <> snd (rsplitWidth (cpreWidth - textWidth ellipsis) cpre)
      (_, Just here)  -> loop (getWord here <> " " <> cpre, move ToPreviousWord here)
      _               -> cpre

  csuf = flip fix ("", move ToNextWord here) $ \loop (csuf, moved) ->
    case (textWidth csuf `compare` csufWidth, moved) of
      (GT, _)         -> fst (splitWidth (csufWidth - textWidth ellipsis) csuf) <> ellipsis
      (_, Just here)  -> loop (csuf <> " " <> getWord here, move ToNextWord here)
      _               -> csuf

numLeftEdges, numRightEdges :: Int
numLeftEdges = Vector.length introLeftEdges
numRightEdges = Vector.length introRightEdges

introLeftEdges, introRightEdges :: Vector String
introLeftEdges   = Vector.fromList $ "" : map return "▕▐" 
introRightEdges  = Vector.fromList $ "" : map return "▏▎▍▌▋▊▉"

-- Use variables, rather than literals, to protect against typos
wordPrefix, wordFocus, wordSuffix, contextPrefix, contextSuffix :: AttrName
wordPrefix  = "wordPrefix"
wordFocus   = "wordFocus"
wordSuffix  = "wordSuffix"
contextPrefix = "contextPrefix"
contextSuffix = "contextSuffix"

cueStart :: Environment -> State -> EventM Name State 
cueStart env@Env{..} st@St{..} = do
  let next | isJust introDuration = startIntro env st
           | otherwise            = reading env wpm
  mode <- next =<< getCurrentTime
  return St{ .. }

handleEvent :: Environment -> State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent env@Env{..} st@St{ ..} (AppEvent Advance{..}) = case mode of 
  -- check the source of the event against the current timer to 
  -- make sure it's not leftover from a prior state
  Introing{..} | source == timer -> do
    -- threadDelay only guarantees a minimum delay, other factors
    -- may cause the thread to wait longer, so see which frame 
    -- we should be on now.
    currTime <- getCurrentTime
    let frameNum = round $ (currTime - startTime) * framesPerMicrosecond
    mode <- if frameNum <= lastFrame
      then do
        timer <- setTimer env (1 / framesPerMicrosecond)
        return Introing{..}
      else reading env wpm currTime
    continue St{..}
  Unpausing{..} | source == timer -> do
    -- threadDelay only guarantees a minimum delay, other factors
    -- may cause the thread to wait longer, so see which frame 
    -- we should be on now.
    currTime <- getCurrentTime
    let frameNum = round $ (currTime - startTime) * framesPerMicrosecond
    mode <- if frameNum < lastFrame
      then do
        timer <- setTimer env (1 / framesPerMicrosecond)
        return Unpausing{..}
      else reading env wpm currTime
    continue St{..}
  Reading{..} | source == timer ->
    let step = case direction of 
          Forwards  -> ToNextWord
          Backwards -> ToPreviousWord
    in case move step here of 
      Nothing -> halt st
      Just here -> do
        mode <- reading env wpm =<< getCurrentTime
        continue St{..}
  _ -> continue st
handleEvent env st@St{..} (VtyEvent ev) = case ev of
  Vty.EvLostFocus -> continue st { mode=Paused{showHelp=False} }
  Vty.EvResize reticuleWidth _rows -> 
    let prefixWidth = orp (Text.replicate reticuleWidth "X")
        suffixWidth = reticuleWidth - 1 - prefixWidth
    in continue St{..}
  Vty.EvKey (flip Map.lookup bindings -> Just action) [] -> action env st
  _ -> continue st
handleEvent _env st _ev = continue st

setTimer :: MonadIO m => Environment -> Microseconds -> m ThreadId
setTimer Env{..} ms = liftIO . forkIO $ do
  threadDelay $ round ms
  writeBChan events . Advance =<< myThreadId

startIntro :: MonadIO m => Environment -> State -> Microseconds -> m Mode
startIntro env@Env{..} St{..} startTime  = do
  let lastFrame = lcm (prefixWidth * numLeftEdges) (suffixWidth * numRightEdges)
      framesPerMicrosecond = fromIntegral lastFrame  / fromMaybe 0 introDuration
  timer <- setTimer env (1 / framesPerMicrosecond)
  return Introing { frameNum = 0, .. }

reading :: MonadIO m => Environment -> WPM -> Microseconds -> m Mode
reading env wpm lastMove = do
  timer <- setTimer env (60e6 / wpm)
  return Reading{ .. }

startUnpause :: MonadIO m => Environment -> State -> Microseconds -> m Mode
startUnpause env@Env{..} St{..} startTime  = do
  let (pre, c, suf) = getWordORP here
      lastFrame = lcm (prefixWidth - textWidth pre) (suffixWidth - textWidth suf + 1 - wcwidth c)
      framesPerMicrosecond = fromIntegral lastFrame / fromMaybe 0 unpauseDuration
  timer <- setTimer env (1 / framesPerMicrosecond)
  return Unpausing { frameNum = 0, .. }

togglePause :: Command
togglePause env@Env{..} st@St{..} = case mode of
  Paused _ -> do
    let next | isJust unpauseDuration = startUnpause env st
             | otherwise              = reading env wpm
    mode <- next =<< getCurrentTime
    continue St{..}
  _ -> continue St{ mode = Paused False, .. }

quit :: Command
quit _ = halt

pauseAndToggleHelp :: Command
pauseAndToggleHelp _ St{..} = continue St
  { mode = case mode of
      Paused True -> Paused False
      _           -> Paused True
  , ..
  }

chain :: Monad m => Int -> (a -> m a) -> a -> m a
chain n = foldr (>=>) return . replicate n

jumpFiveSeconds :: Direction -> Command
jumpFiveSeconds dir _env st@St{..} = 
  let step = if dir == direction
        then ToNextWord
        else ToPreviousWord
      numSteps = max 1 $ round (wpm/12)
  in case chain numSteps (move step) here of
    Nothing -> halt st
    Just here -> continue st{ here = here }

jumpOneSentence :: Direction -> Command
jumpOneSentence dir _env st@St{..} = 
  let step = if dir == direction
        then ToNextSentence
        else ToPreviousSentence 
  in case move step here of
    Nothing -> halt st
    Just here -> continue st{ here = here }

jumpOneParagraph :: Direction -> Command
jumpOneParagraph dir _env st@St{..} = 
  let step = if dir == direction
        then ToNextParagraph
        else ToPreviousParagraph 
  in case move step here of
    Nothing -> halt st
    Just here -> continue st{ here = here }

toggleReadingDirection :: Command
toggleReadingDirection _ St{..} = continue St
  { direction = case direction of 
      Forwards -> Backwards
      Backwards -> Forwards
  , ..
  }

pauseAndStepOneWord :: Direction -> Command
pauseAndStepOneWord dir _env St{..} = continue St
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
increaseWPM _env st@St{..} = continue st{ wpm = wpm + 1 }

decreaseWPM :: Command
decreaseWPM _env st@St{..} = continue st{ wpm = wpm - 1 }

pauseAndChangeProgress :: Direction -> Command
pauseAndChangeProgress dir _env st@St{..} = continue st
  { mode = case mode of
      Paused _  -> mode
      _         -> Paused False
  , progressDisplay = if dir == Forwards
      then case progressDisplay of { FractionWords -> Percentage ; _ -> succ progressDisplay }
      else case progressDisplay of { Percentage -> FractionWords ; _ -> pred progressDisplay }
  }
