{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.Semigroup ((<>))
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector
import System.Exit (exitFailure)
import System.IO (stderr)

import Brick
  ( AttrMap, attrMap
  , App(..), neverShowCursor
  , Widget
  , BrickEvent
  , Next
  , EventM, halt
  , customMain
  )
import qualified Brick
import Brick.BChan 
  ( BChan, newBChan, writeBChan
  )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Options.Applicative 
  ( ParserInfo, info, option, argument, auto, str, long, help, showDefault
  , value, metavar, helper, fullDesc, progDesc, header, execParser, (<**>)
  , switch
  )
import System.Clock
import qualified Graphics.Vty as Vty

import Cursor
import Document
import Position (Position(..))

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings
type Microseconds = Double

data Env = Env
  { startDuration   :: Maybe Microseconds
  , unpauseDuration :: Maybe Microseconds
  , reticuleWidth   :: Int
  , events          :: BChan Event
  , attributes      :: AttrMap
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
  = Introing { timer :: !ThreadId, frameNum :: Int, startTime :: !Microseconds }
  | Reading { timer :: !ThreadId, lastWord :: !Microseconds }
  | Paused { showHelp :: !Bool }
  | Unpausing { timer :: !ThreadId, frameNum :: Int, startTime :: !Microseconds }
  | Finished

data Event
  = Advance

{-
data Action
  = Start
  | Quit
  | Pause
  | Resume
  | Help
  | IncreaseSpeed
  | DecreaseSpeed
  | PreviousWord
  | PreviousSentence
  | PreviousParagraph
  | NextWord
  | NextSentence
  | NextParagraph
  -}

data Options = Options
  { initialWpm  :: Double
  , initialPos  :: Position
  , path        :: String
  , skipIntro   :: Bool
  }
  deriving Show

options :: ParserInfo Options
options = info 
  (   Options <$> option auto 
                  (   long "wpm"
                  <>  help "Words to print per minute"
                  <>  showDefault
                  <>  value 250
                  <>  metavar "N"
                  )
              <*> option auto
                  (   long "pos"
                  <>  help "Starting position in the text"
                  <>  showDefault
                  <>  value (Position 0 0 0)
                  <>  metavar "PARAGRAPH.SENTENCE.WORD"
                  )
              <*> argument str
                  (   help "File to read from"
                  <>  metavar "PATH"
                  )
              <*> switch
                  (   long "skip-intro"
                  <>  help "Skip the introductory countdown"
                  )
  <**> helper
  )
  (   fullDesc
  <>  progDesc "Print a file one word at a time at a given wpm"
  <>  header "spreto - a speed-reading tool"
  )

-- |
-- Approximate the "optimal recognition point" (ORP) for a given word,
-- the character that should be aligned and highlighted when the word is
-- printed.
--
-- >>> orp "a"
-- 0
-- >>> orp "to"
-- 1
-- >>> orp "now"
-- 1
-- >>> orp "word"
-- 1
-- >>> orp "using"
-- 1
-- >>> orp "slower"
-- 2
-- >>> orp "reading"
-- 2
-- >>> orp "provides"
-- 2
-- >>> orp "delivered"
-- 2
-- >>> orp "technology"
-- 3
orp :: Text -> Int
-- XXX: Expect to spend a lot of time tweaking this
--
-- There examples seem mostly word-length based, but it
-- also seems like capital letters are weighed extra, while
-- common suffices (e.g. -ing, -ed) are weighed less.
--
-- We'll probably also want to account for punctuation.
--
-- We/lcome t/o sp/ritzing!
-- R/ight n/ow y/ou a/re u/sing o/ur inn/ovative re/ading tec/hnology 
-- E/ach w/ord i/s de/livered t/o y/our e/yes i/n t/he pe/rfect po/sition 
-- In/stead o/f y/ou 
orp w = (Text.length w + 2) `div` 4

{-

action :: Mode -> Char -> Maybe Action
action Initial _ = Just Start
action Final _ = Nothing
action Running ' ' = Just Pause
action _ ' ' = Just Resume
action _ 'h' = Just Help
action _ '?' = Just Help
-- \ESC[A up
-- \ESC[B down
-- \ESC[C right
-- \ESC[D left
action _ 'p' = Just PreviousParagraph
action _ 'P' = Just NextParagraph
action _ 's' = Just PreviousParagraph
action _ 'S' = Just NextParagraph
  -}

getCurrentTime :: IO Microseconds
getCurrentTime = do
  TimeSpec{..} <- getTime Monotonic
  return $ 1000*1000*fromIntegral sec + fromIntegral nsec / 1000

orElseM :: Monad m => Maybe a -> m a -> m a
orElseM (Just a) _ = return a
orElseM _ ma       = ma

-- | Named Resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

makeApp :: Env -> App State Event Name
makeApp env = App
  { appDraw         = draw env
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent env
  , appStartEvent   = return
  , appAttrMap      = const (attributes env)
  }

draw :: Env -> State -> [Widget Name]
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
      [ Brick.str leftEdge
      , Brick.str $ replicate numFull '█'
      , Brick.str rightEdge
      ]
      
  drawState State{ mode=Reading{..}, ..} = 
    let word = getWord here
        n = orp word
        (pre,Just (c, suf)) = Text.uncons <$> Text.splitAt n word
    in 
    reticule (prefixWidth - n)
      [ Brick.withAttr wordPrefix $ Brick.txt pre
      , Brick.withAttr wordFocus  $ Brick.str [c]
      , Brick.withAttr wordSuffix $ Brick.txt suf
      ] 

  drawState State{ mode=Paused{..} } = Brick.str "Paused"
  drawState State{ mode=Unpausing{..} } = Brick.str "Unpausing"
  drawState State{ mode=Finished } = Brick.str "Finished"

  reticule n xs = Brick.vBox
    [ Brick.str reticuleTop
    , Brick.padLeft (Brick.Pad n) (Brick.hBox xs)
    , Brick.str reticuleBottom
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
wordPrefix, wordFocus, wordSuffix :: Brick.AttrName
wordPrefix = "wordPrefix"
wordFocus = "wordFocus"
wordSuffix = "wordSuffix"

handleEvent :: Env -> State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent Env{..} = handleStateEvent where

  handleStateEvent st@State{ mode=Introing{..}, ..} = \case
    Brick.AppEvent Advance | frameNum < lastFrame -> do
      let nextTime = startTime + (fromIntegral frameNum + 1)/framesPerMicrosecond
      timer <- liftIO . forkIO $ do
        currTime <- getCurrentTime
        threadDelay $ round (nextTime - currTime)
        writeBChan events Advance
      Brick.continue st { mode=Introing{ frameNum = frameNum + 1,..} }

    Brick.AppEvent Advance | frameNum == lastFrame -> do
      lastWord <- liftIO $ getCurrentTime
      timer <- liftIO . forkIO $ do
        threadDelay $ round (60e6 / wpm)
        writeBChan events Advance
      Brick.continue st { mode=Reading{..} }

    _ -> Brick.continue st

  handleStateEvent st@State{ mode=Reading{..}, ..} = \case
    Brick.AppEvent Advance -> case move ToNextWord here of
      Just here -> do
        lastWord <- liftIO $ getCurrentTime
        timer <- liftIO . forkIO $ do
          threadDelay $ round (60e6 / wpm)
          writeBChan events Advance
        Brick.continue State { mode=Reading{..}, .. }
      Nothing -> halt st
    _ -> halt st
  handleStateEvent st = \_ -> halt st

  prefixWidth = orp (Text.replicate reticuleWidth "X")
  suffixWidth = reticuleWidth - 1 - prefixWidth

  lastFrame = lcm (prefixWidth * numLeftEdges)
                  (suffixWidth * numRightEdges)

  introDurationInMicroseconds = 5e6
  framesPerMicrosecond = fromIntegral lastFrame  / introDurationInMicroseconds

main :: IO ()
main = do
  Options{..} <- execParser options
  let startDuration   = if skipIntro then Nothing else Just 5e6
      unpauseDuration = if skipIntro then Nothing else Just 5e6
      wpm             = initialWpm
      reticuleWidth   = 80 -- Q: why 80? A: 80 columns is a "standard" code width
      direction       = Forwards
      attributes      = attrMap Vty.defAttr [(wordFocus, Brick.fg Vty.red)]
  document <- parseDocument <$> TextIO.readFile path
  here <- cursor document initialPos `orElseM` do
    TextIO.hPutStrLn stderr . Text.pack $ "ERROR: illegal start position " <> show initialPos <> " in document " <> path
    exitFailure
  events <- newBChan 10 -- Q: why 10? A: stolen from the example
  startTime <- getCurrentTime
  timer <- forkIO $ writeBChan events Advance
  let mode | skipIntro = Reading { lastWord = startTime - 60e6 / wpm, ..}
           | otherwise = Introing { frameNum = 0, .. }
  -- XXX: takes over the entire display, which is non-optimal
  --      see https://github.com/jtdaugherty/vty/issues/143
  void $ customMain 
          (Vty.mkVty Vty.defaultConfig) 
          (Just events) 
          (makeApp Env{..}) 
          State{..} 
  -- clear the bottom line if exit after intro
  putStrLn ""
  putStrLn ""
