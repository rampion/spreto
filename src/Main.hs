{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Options.Applicative
import Data.Semigroup ((<>))

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

main :: IO ()
main = do
  r <- execParser options
  print r
