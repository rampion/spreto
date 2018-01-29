{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Prelude hiding (words)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import Control.Concurrent
import Control.Exception.Base (bracket)
import System.IO
import TextIndex
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector

import Position

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings

{-
  -- sync via MVar?
data State
  { frameTimer  :: !ThreadId
  , lastFrame   :: !Time
  , fps         :: !Double
  , direction   :: !Direction
  , mode        :: !Mode
  , current     :: !Position
  }
-}

data Mode 
  = Starting
  | Reading
  | Paused { withHelp :: !Bool }
  | Unpausing
  | Done


data Options = Options
  { wpm :: Float
  , start :: Position
  , path :: String
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
  <**> helper
  )
  (   fullDesc
  <>  progDesc "Print a file one word at a time at a given wpm"
  <>  header "spreto - a speed-reading tool"
  )

hideCursor, showCursor :: IO ()
hideCursor = putStr "\ESC[?25l" >> hFlush stdout
showCursor = putStr "\ESC[?25h" >> hFlush stdout

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

intro :: IO ()
intro = do
  let n = lcm (59*8) (20*3)
      runtimeInMicroseconds = 3000000
      microsecondsPerBar = runtimeInMicroseconds `div` n
      lefts  = "" : map return "▏▎▍▌▋▊▉" 
      rights = "" : map return "▕▐"
  forM_ [n,n-1..0] $ \i -> do
    let (rfull,rpart) = (20 * i) `quotRem` n
        (lfull,lpart) = (59 * i) `quotRem` n
        full = rfull + lfull + 1
        prefix = rights !! quot (3 * rpart) n
        suffix = lefts  !! quot (8 * lpart) n
        indent = 20 - rfull - if null prefix then 0 else 1
    putStrLn $ concat
      -- \ESC[<N>C - move cursor N columns right
      [ "\ESC[F\ESC[K\ESC[", show indent, "C"
      , prefix, replicate full '█', suffix
      ]
    threadDelay microsecondsPerBar

(#) :: Text -> (Int, Int) -> Text
t # (lo, hi) = Text.take (hi - lo) $ Text.drop lo t

main :: IO ()
main = do
  Options{..} <- execParser options
  TextIndex{..} <- textIndex <$> Text.readFile path
  bracket hideCursor (const showCursor) $ \_ -> do
    putStrLn "────────────────────┬───────────────────────────────────────────────────────────"
    putStrLn ""
    -- \ESC[F - move cursor to beginning of previous line
    putStrLn "────────────────────┴───────────────────────────────────────────────────────────\ESC[F"
    intro
    let delay = round (60 * 1000000 / wpm)
    Vector.forM_ words $ \word -> do
      let n = orp word
      let (pre,Just (c, suf)) = Text.uncons <$> Text.splitAt n word
      Text.putStrLn $ Text.concat
        [ "\ESC[F"
        -- \ESC[K - clear to end of line
        , "\ESC[K"
        -- \ESC[C - move cursor right
        -- to align ORP character with reticule
        , "\ESC[", Text.pack (show (20 - n)), "C"
        , pre
        -- \ESC[31m - highlight ORP character in red
        , "\ESC[31m"
        , Text.singleton c
        , "\ESC[m"
        , suf
        ]
      threadDelay delay
