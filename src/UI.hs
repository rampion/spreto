{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.List (intercalate)
import Data.Semigroup ((<>))

import Brick hiding (Direction)
import Brick.BChan (BChan, writeBChan)
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

import ORP (orp)
import Cursor (Cursor, move, Increment(..), getWord, getPosition, getDocument)
import Document (numParagraphs, numSentences, numWords)
import Position (Position(..))

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings
type Microseconds = Double
type WPM = Double
type FrameIndex = Int
type Columns = Int

-- | environment parameters given by the user
data Environment = Env
  { introDuration   :: Maybe Microseconds
  , unpauseDuration :: Maybe Microseconds
  , events          :: BChan Event
  , attributes      :: AttrMap
  }

data State = St
  { wpm           :: !WPM       -- ^ rate at which to show words
  , direction     :: !Direction -- ^ reading forwards or backwards
  , here          :: !Cursor    -- ^ current position in the text
  , progress      :: !Progress  -- ^ how to show progress when paused
  , reticule      :: !Reticule
  , mode          :: !Mode
  }

data Direction
  = Forwards
  | Backwards
  deriving (Eq)

data Progress
  = Percentage
  | FractionTime
  | FractionParagraphs
  | FractionSentences
  | FractionWords
  deriving Enum

-- | derived from the environment parameters
--   and the rendering context
data Reticule = Reticule
  { prefixWidth  :: !Columns
  , suffixWidth  :: !Columns
  , drawReticule :: !(Columns -> [Widget Name] -> Widget Name)
  }

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

-- | Named Resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
data Name = Name deriving (Eq, Ord)

type Command = Environment -> State -> EventM Name (Next State)
type Binding = (Vty.Key, Command)

makeApp :: Environment -> App State Event Name
makeApp env = App
  { appDraw         = draw env
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent env
  , appStartEvent   = cueStart env
  , appAttrMap      = const (attributes env)
  }

makeReticule :: Environment -> Columns -> Reticule
makeReticule Env{..} reticuleWidth = Reticule{..} where
  prefixWidth = orp (Text.replicate reticuleWidth "X")
  suffixWidth = reticuleWidth - 1 - prefixWidth
  reticuleTop     = reticuleLine '┬'
  reticuleBottom  = reticuleLine '┴'
  reticuleLine c = replicate prefixWidth '─' ++ c : replicate suffixWidth '─'

  drawReticule n xs = vBox
    [ str reticuleTop
    , padLeft (Pad n) (hBox xs)
    , str reticuleBottom
    ]

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
  , ("pause and change progress display"
    , [ (Vty.KChar '\t', pauseAndChangeProgress Forwards)
      , (Vty.KBackTab, pauseAndChangeProgress Backwards)
      ]
    )
  ]

bindings :: Map Vty.Key Command
bindings = Map.fromList $ snd =<< usage

getCurrentTime :: MonadIO m => m Microseconds
getCurrentTime = liftIO $ do
  TimeSpec{..} <- getTime Monotonic
  return $ 1000*1000*fromIntegral sec + fromIntegral nsec / 1000

splitWidth :: Int -> Text -> (Text, Text)
splitWidth n t = case Text.uncons t of
  Just (c, t) | wcwidth c <= n -> 
    let ~(xs,ys) = splitWidth (n - wcwidth c) t
    in (Text.cons c xs, ys)
  _ -> ("", t)

rsplitWidth :: Int -> Text -> (Text, Text)
rsplitWidth n t = case Text.unsnoc t of
  Just (t, c) | wcwidth c <= n -> 
    let ~(xs,ys) = rsplitWidth (n - wcwidth c) t
    in (xs, Text.snoc ys c)
  _ -> (t, "")

getWordORP :: Cursor -> (Text, Char, Text)
getWordORP here = (pre, c, suf) where
  word = getWord here
  (pre,Just (c, suf)) = Text.uncons <$> splitWidth (orp word) word

draw :: Environment -> State -> [Widget Name]
draw Env{..} St{..} = case mode of
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
    return $ drawReticule indent 
      [ str leftEdge
      , str $ replicate numFull '█'
      , str rightEdge
      ]
      
  Reading{..} ->
    let (pre, c, suf) = getWordORP here in 
    return $ drawReticule (prefixWidth - textWidth pre)
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
    let (cpre, wpre, wfoc, wsuf, csuf) = context reticule here
        doc = getDocument here
        pos@Position{..} = getPosition here
        absSentenceNum = sentenceNum + sum [numSentences doc p | p <- [0..paragraphNum-1]]
        absNumSentences = Vector.sum $ Vector.length <$> doc
        absWordNum = wordNum 
          + sum [numWords doc paragraphNum s | s <- [0..sentenceNum-1]]
          + sum [numWords doc p s | p <- [0..paragraphNum-1], s <- [0..numSentences doc p - 1]]
        absNumWords = Vector.sum $ (Vector.sum . fmap Vector.length) <$> doc

        percent :: Int
        percent = round (100 * fromIntegral absWordNum / fromIntegral absNumWords :: Double)

        (estHours, estMinutes, estSeconds) = toTime absWordNum
        (totHours, totMinutes, totSeconds) = toTime absNumWords

        toTime :: Int -> (Int,Int,Int)
        toTime n = (h,m,s) where
          hms = round $ 60 * fromIntegral n / wpm
          (hm,s) = hms `quotRem` 60
          (h,m)  = hm `quotRem` 60

        tshow n | n < 10 = '0' : show n
                | otherwise = show n

        spos = show pos
        reticuleWidth = prefixWidth + 1 + suffixWidth
        rightJustify = reticuleWidth - wcswidth spos
    in
    [ drawReticule (prefixWidth - textWidth cpre - textWidth wpre)
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
    , translateBy (Location (rightJustify,3)) $ str spos
    , padTop (Pad 3) $ hCenter $ str $ case progress of
      FractionParagraphs -> concat
        [ show paragraphNum 
        , "/"
        , show $ numParagraphs doc
        , " paragraphs"
        ]
      FractionSentences -> concat
        [ show absSentenceNum
        , "/"
        , show $ absNumSentences
        , " sentences"
        ]
      FractionWords -> concat
        [ show absWordNum
        , "/"
        , show $ absNumWords
        , " words"
        ]
      Percentage -> concat [ show percent, "%" ]
      FractionTime -> concat
        [ show estHours, ":", tshow estMinutes, ":", tshow estSeconds
        , "/"
        , show totHours, ":", tshow totMinutes, ":", tshow totSeconds
        ]
    , if showHelp
        then padTop (Pad 5) $ hBox $ padLeft (Pad 4) <$>
          [ vBox [ str keys | (_, unzip -> (showKeys -> keys, _)) <- usage ]
          , vBox [ str helpMsg | (helpMsg, _) <- usage ]
          ]
        else emptyWidget
    ]

  Unpausing{..} -> 
    let (cpre, wpre, wfoc, wsuf, csuf) = context reticule here
        framesRemaining = lastFrame - frameNum
        leftFull = ((prefixWidth - textWidth wpre) * framesRemaining) `quot` lastFrame
        rightFull = ((suffixWidth - textWidth wsuf) * framesRemaining) `quot` lastFrame
        cpre' = snd $ rsplitWidth leftFull cpre
        csuf' = fst $ splitWidth rightFull csuf
    in
    [ drawReticule (prefixWidth - textWidth cpre' - textWidth wpre)
      [ withAttr contextPrefix . txt $ cpre'
      , withAttr wordPrefix $ txt wpre
      , withAttr wordFocus $ str [wfoc]
      , withAttr wordSuffix $ txt wsuf
      , withAttr contextSuffix . txt $ csuf'
      ]
    ]
    -- cropLeftBy / cropRightBy

  where Reticule{..} = reticule

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

context :: Reticule -> Cursor -> (Text, Text, Char, Text, Text)
context Reticule{..} here = (cpre, wpre, wfoc, wsuf, csuf) where
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
handleEvent env st (VtyEvent ev) = case ev of
  Vty.EvLostFocus -> continue st { mode=Paused{showHelp=False} }
  Vty.EvResize cols _rows -> continue st { reticule=makeReticule env cols }
  Vty.EvKey (flip Map.lookup bindings -> Just action) [] -> action env st
  _ -> continue st
handleEvent _env st _ev = continue st

setTimer :: MonadIO m => Environment -> Microseconds -> m ThreadId
setTimer Env{..} ms = liftIO . forkIO $ do
  threadDelay $ round ms
  writeBChan events . Advance =<< myThreadId

startIntro :: MonadIO m => Environment -> State -> Microseconds -> m Mode
startIntro env@Env{..} St{..} startTime  = do
  let Reticule{..} = reticule
      lastFrame = lcm (prefixWidth * numLeftEdges) (suffixWidth * numRightEdges)
      framesPerMicrosecond = fromIntegral lastFrame  / fromMaybe 0 introDuration
  timer <- setTimer env (1 / framesPerMicrosecond)
  return Introing { frameNum = 0, .. }

reading :: MonadIO m => Environment -> WPM -> Microseconds -> m Mode
reading Env{..} wpm lastMove = do
  timer <- liftIO . forkIO $ do
    threadDelay $ round (60e6 / wpm)
    writeBChan events . Advance =<< myThreadId
  return Reading{ .. }

startUnpause :: MonadIO m => Environment -> State -> Microseconds -> m Mode
startUnpause env@Env{..} St{..} startTime  = do
  let Reticule{..} = reticule
      (pre, c, suf) = getWordORP here
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
  , progress = if dir == Forwards
      then case progress of { FractionWords -> Percentage ; _ -> succ progress }
      else case progress of { Percentage -> FractionWords ; _ -> pred progress }
  }
