{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cursor 
{-
  ( Cursor(), cursor
  , currentWord, currentPosition
  , nextWord, previousWord
  , nextSentence, previousSentence
  , nextParagraph, previousParagraph
  )
  -}
  where
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Position
import TextIndex

data Cursor = Cursor
  { document :: Document
  , position :: Position
  }

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :{
-- oz = textIndex $ Text.unlines
--   [ "Suddenly Uncle Henry stood up."
--   , ""
--   , "\"There's a cyclone coming, Em,\" he called to his wife. \"I'll go look"
--   , "after the stock.\" Then he ran toward the sheds where the cows and horses"
--   , "were kept."
--   , ""
--   , "Aunt Em dropped her work and came to the door. One glance told her of"
--   , "the danger close at hand."
--   , ""
--   , "\"Quick, Dorothy!\" she screamed. \"Run for the cellar!\""
--   ]
-- :}

-- |
-- >>> Cursor oz (Position 0 0 0)
-- 0.0.0 → Suddenly
-- >>> Cursor oz (Position 1 2 3)
-- 1.2.3 → toward
-- >>> Cursor oz (Position 3 2 3)
-- 3.2.3 → cellar!"
instance Show Cursor where
  showsPrec _ c 
    = shows (currentPosition c)
    . showString " → "
    . showString (Text.unpack $ currentWord c)

-- |
-- >>> firstPosition . Cursor oz $ Position 1 2 3
-- 0.0.0
firstPosition :: Cursor -> Position
firstPosition _ = Position 0 0 0

(!) :: Vector a -> Word -> a
v ! i = v Vector.! fromIntegral i

-- |
-- >>> lastPosition . Cursor oz $ Position 1 2 3
-- 3.2.3
lastPosition :: Cursor -> Position
lastPosition (Cursor {..}) = Position{..} where
  paragraph = Vector.length document - 1
  sentence = Vector.length (document ! paragraph) - 1
  word = Vector.length (document ! paragraph ! sentence) - 1

wordPercentage, sentencePercentage, paragraphPercentage :: Cursor -> Float
wordPercentage = undefined
sentencePercentage = undefined
paragraphPercentage = undefined

-- | smart constructor for Cursor, normalizing positions
--
-- >>> cursor oz $ Position 0 0 0
-- 0.0.0 → Suddenly
-- >>> cursor oz $ Position 0 0 24
-- 1.2.3 → toward
-- >>> cursor oz $ Position 1 4 17
-- 3.2.3 → cellar!"
cursor :: Document -> Position -> Maybe Cursor
cursor document = \Position{..} -> Cursor paragraphs <$> checkP paragraph sentence word where
  checkP p s w
    | p < 0 || maxP <= p = Nothing
    | otherwise          = checkS p s w
    where maxP = Vector.length document

  checkS p s w
    | s < 0     = checkP (p - 1) (s + maxS') w
    | s >= maxS = checkP (p + 1) (s - maxS) w
    | otherwise = checkW p s w
    where maxS = Vector.length (document ! p)
          maxS' = Vector.length (document ! (p-1))


  search p s w
    | p < 0 || maxP <= p        = Nothing
    | s < 0 && 1 <= p           = search (p - 1) (s + maxS') w
    | s >= maxS && p + 1 < maxP = search (p + 1) (s - maxS) w
    | w < 0 && 1 <= s           = search p (s - 1) (w + maxW)
    | w < 0 && 0 == s           = search (p - 1) (
    | 
    where maxS    = Vector.length (document ! p)
          maxS'   = Vector.length (document ! (p - 1))
          maxW    = Vector.length (document ! p ! s)
          maxW'   = Vector.length (document ! p ! (s - 1))
          maxW''  = Vector.length (document ! (p - 1) ! (maxS' - 1))
  guard $ paragraph < numParagraphs
  guard $ 
  





      absSentence = sentence + fst (paragraphs ! paragraph)
      absWord = word + fst (sentences ! absSentence)

      (word', absSentence') = findRelative absWord absSentence numSentences sentences
      (sentence', paragraph') = findRelative absSentence' paragraph numParagraphs paragraphs

      -- check if people tell the truth, fall back on binary search
      findRelative i lo hi v 
        | mini <= i && i < maxi = (i - mini, lo)
        | otherwise             = findRelativeB i (lo+1) hi v
        where (mini, maxi) = v ! lo

      findRelativeB i lo hi v
        | i < mini  = findRelativeB i lo (j-1) v
        | maxi <= i = findRelativeB i (j+1) hi v
        | otherwise = (i - mini, j)
        where j = (lo + hi) `div` 2
              (mini, maxi) = v ! j

  guard $   paragraph < numParagraphs 
        &&  absSentence < numSentences
        &&  absWord < numWords

  return $ Position paragraph' sentence' word'

currentWord :: Cursor -> Text
currentWord (Cursor TextIndex{..} Position{..}) = words ! wordNum where
  wordNum = word + minWord
  (minWord, _) = sentences ! sentenceNum
  sentenceNum = sentence + minSentence
  (minSentence, _) = paragraphs ! paragraph

nextWord, previousWord, nextSentence, previousSentence, nextParagraph, previousParagraph
  :: Cursor -> Maybe Cursor
nextWord = undefined
previousWord = undefined
nextSentence = undefined
previousSentence = undefined
nextParagraph = undefined
previousParagraph = undefined
