module Cursor 
  ( Cursor(), cursor
  , currWord, currPosition
  , nextWord, prevWord
  , nextSentence, prevSentence
  , nextParagraph, prevParagraph
  )
  where
import Data.Text (Text)
import qualified Data.Text as Text

import Position
import TextIndex

data Cursor = Cursor
  { textIndex :: TextIndex
  , currPosition :: Position
  }

-- $setup
-- >>> :{
-- let oz = unlines
--       [ "Suddenly Uncle Henry stood up."
--       , ""
--       , "\"There's a cyclone coming, Em,\" he called to his wife. \"I'll go look"
--       , "after the stock.\" Then he ran toward the sheds where the cows and horses"
--       , "were kept."
--       , ""
--       , "Aunt Em dropped her work and came to the door. One glance told her of"
--       , "the danger close at hand."
--       , ""
--       , "\"Quick, Dorothy!\" she screamed. \"Run for the cellar!\""
--       ]
-- :}

-- |
-- >>> :m +Data.Text
-- >>> Data.Text.putStr oz
-- Suddenly Uncle Henry stood up.
-- <BLANKLINE>
-- "There's a cyclone coming, Em," he called to his wife. "I'll go look
-- after the stock." Then he ran toward the sheds where the cows and horses
-- were kept.
-- <BLANKLINE>
-- Aunt Em dropped her work and came to the door. One glance told her of
-- the danger close at hand.
-- <BLANKLINE>
-- "Quick, Dorothy!" she screamed. "Run for the cellar!"
-- >>> Cursor oz (Position 0 0 0)
-- "Suddenly" :@ Position 0 0 0
-- >>> Cursor oz (Position 2 2 3)
-- "toward" :@ Position 1 2 3
-- >>> Cursor oz (Position 3 1 3)
-- "cellar!\"" :@ Position 3 1 3
instance Show Cursor where
  showsPrec _ c 
    = shows (Text.unpack $ currWord c)
    . showString " :@ "
    . shows (currPosition c)

firstPosition, lastPosition :: Cursor -> Position
firstPosition = undefined
lastPosition = undefined

wordPercentage, sentencePercentage, paragraphPercentage :: Cursor -> Float
wordPercentage = undefined
sentencePercentage = undefined
paragraphPercentage = undefined

-- [0.0.0]---------------------------(1.2.3)---[12.0.3]
-- | smart constructor for Cursor
--
-- [1.2.3] / [12.0.3]
--
cursor :: TextIndex -> Position -> Maybe Cursor
cursor ix pos = undefined -- Cursor ix <$> normalize ix pos

currWord :: Cursor -> Text
currWord = undefined

nextWord, prevWord, nextSentence, prevSentence, nextParagraph, prevParagraph
  :: Cursor -> Maybe Cursor
nextWord = undefined
prevWord = undefined
nextSentence = undefined
prevSentence = undefined
nextParagraph = undefined
prevParagraph = undefined
