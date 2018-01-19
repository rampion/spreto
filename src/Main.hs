{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import Control.Concurrent
import Control.Exception.Base (bracket)
import System.IO

data Position = Position
  { paragraph :: Word
  , sentence :: Word
  , word :: Word
  }

-- |
-- >>> Position 409 0 1
-- 409.0.1
instance Show Position where
  showsPrec _ Position{..}
    = shows paragraph . showString "." . shows sentence . showString "." . shows word


-- |
-- >>> paragraph $ read "409.0.1"
-- 409
-- >>> sentence $ read "409.0.1"
-- 0
-- >>> word $ read "409.0.1"
-- 1
instance Read Position where
  readsPrec _ = go where
    go ( splitDot -> (reads -> [(paragraph, "")]
       , splitDot -> (reads -> [(sentence, "")]
       ,              reads -> [(word, "")])))
      = [(Position{..}, "")]
    go _ = []

    splitDot = fmap tail <$> break ('.'==)

data Options = Options
  { wpm :: Float
  , position :: Position
  , inputPath :: String
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
orp :: String -> Int
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
orp w = (length w + 2) `div` 4

main :: IO ()
main = bracket hideCursor (const showCursor) $ \_ -> do
  Options{..} <- execParser options
  ws <- words <$> readFile inputPath
  putStrLn "───────────────────┬─────────────────────────────────────────────────"
  putStrLn ""
  -- \ESC[F - move cursor to beginning of previous line
  putStr   "───────────────────┴─────────────────────────────────────────────────\ESC[F"
  hFlush stdout
  let delay = round (60 * 1000000 / wpm)
  forM_ ws $ \w -> do
    let n = orp w
    let ~(xs,y:ys) = splitAt n w
    -- \ESC[G - move to beginning of line
    -- \ESC[K - clear to end of line
    -- \ESC[31m - change foreground color to red
    -- \ESC[m - reset foreground color
    putStr $ "\ESC[G\ESC[K" ++ replicate (19 - n) ' ' ++ xs ++ "\ESC[31m" ++ [y] ++ "\ESC[m" ++ ys
    hFlush stdout
    threadDelay delay
