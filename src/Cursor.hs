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
import Data.Functor.Compose (Compose(..))
import Data.Traversable (mapAccumL)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

import Position
import Document

data Cursor = Cursor
  { document :: Document
  , currentPosition :: Position
  }

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :{
-- oz = parseDocument $ Text.unlines
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
-- 0.0.0:Suddenly
-- >>> Cursor oz (Position 1 2 3)
-- 1.2.3:toward
-- >>> Cursor oz (Position 3 2 3)
-- 3.2.3:cellar!"
instance Show Cursor where
  showsPrec _ c 
    = shows (currentPosition c)
    . showString ":"
    . showString (Text.unpack $ currentWord c)

currentWord :: Cursor -> Text
currentWord Cursor{ document = document, currentPosition = Position {..}} = document ! paragraph ! sentence ! word

-- |
-- >>> cursor oz (Position 1 (-1) 0)
-- Just 0.0.0:Suddenly
-- >>> cursor oz (Position 0 0 24)
-- Just 1.2.3:toward
-- >>> cursor oz (Position 0 8 3)
-- Just 3.2.3:cellar!"
cursor :: Document -> Position -> Maybe Cursor
cursor d = (Cursor d <$>) . normParagraph where
  normParagraph p@Position{..}
    | paragraph < minParagraph d p || maxParagraph d p < paragraph  = Nothing
    | sentence < 0                          = normSentenceLo p >>= normWord
    | numSentences d paragraph <= sentence  = normSentenceHi p >>= normWord
    | otherwise                             = normWord p

  normWord p@Position{..}
    | word < 0                              = normWordLo p
    | numWords d paragraph sentence <= word = normWordHi p
    | otherwise                             = Just p

  normSentenceLo p = do
    p@Position{..} <- incParagraph d p
    if sentence < 0 then normSentenceLo p else return p

  normSentenceHi p = do
    p@Position{..} <- decParagraph d p
    if numSentences d paragraph <= sentence then normSentenceHi p else return p

  normWordLo p = do
    p@Positon{..} <- incSentence d p
    if word < 0 then normWordLo p else return p

  normWordhi p = do
    p@Position{..} <- decSentence d p
    if numWords d paragraph sentence <= word then normWordhi p else return p

incParagraph :: Document -> Position -> Maybe Position
incParagraph d p@Postion{..}
  | paragraph + 1 < numParagraphs d = Just p { paragraph = paragraph + 1, sentence = sentence - numSentences d paragraph }
  | otherwise                       = Nothing

decParagraph :: Document -> Position -> Maybe Position
decParagraph _ p@Position{..}
  | 0 < paragraph = Just p { paragraph = paragraph - 1, sentence = sentence + numSentences d (paragraph - 1) }
  | otherwise     = Nothing

incSentence :: Document -> Position -> Maybe Position
incSentence d (update -> p@Position{..})
  | sentence + 1 < numSentences d paragraph = Just p { sentence = sentence + 1 }
  | otherwise                               = incParagraph p
  where update d p@Position = p { word = word - numWords d paragraph sentence }

decSentence :: Document -> Position -> Maybe Position
decSentence d p@Position{..}
  | 0 < sentence = Just p { sentence = sentence - 1 } <&> update d
  | otherwise    = decParagraph d p <&> update d
  where update d p@Position{..} = p { word = word + numWords d paragraph sentence }

nextWord :: Document -> Position -> Maybe Position
nextWord d p@Position{..}
  | word + 1 < numWords d paragraph sentence = Just p'
  | otherwise                                = incSentence p'
  where p' = p { word = word + 1 }

prevWord :: Document -> Position -> Maybe Position
prevWord d p@Position{..}
  | 0 < word = Just p'
  | otherwise = decSentence p'
  where p' = p { word = word - 1 }

nextSentence :: Document -> Position -> Maybe Position
nextSentence d p@Position{..}
  | sentence + 1 < numSentences d paragraph = Just p'
  | otherwise                               = incParagraph p'
  where p' = p { sentence = sentence + 1, word = 0 }

prevSentence :: Document -> Position -> Maybe Position
prevSentence d p@Position{..}
  | 0 < sentence = Just p'
  | otherwise = decParagraph p'
  where p' = p { sentence = sentence - 1, word = 0 }

nextParagraph d p@Position{..}
  | paragraph + 1 < numParagraphs d = Just p { paragraph = paragraph + 1, sentence = 0, word = 0 }
  | otherwise = Nothing

prevParagraph d p@Position{..}
  | 0 < paragraph = Just p { paragraph = paragraph - 1, sentence = 0, word = 0 }
  | otherwise = Nothing


-- cursor :: Document -> Position -> Maybe (Word, Position)
cursor document = fmap (Cursor document) . normalize where

  normalize :: Position -> Maybe Position
  normalize position@Position{..}
    | paragraph < 0 || numParagraphs <= paragraph = Nothing
    | sentence < 0                                = normalize position { paragraph = paragraph - 1, sentence = sentence + numSentencesPrior ! paragraph }
    | numSentences ! paragraph <= sentence        = normalize position { paragraph = paragraph + 1, sentence = sentence - numSentences ! paragraph }
    | word < 0                                    = normalize position { sentence = sentence - 1, word = word + numWordsPrior ! paragraph ! sentence }
    | numWords ! paragraph ! sentence <= word     = normalize position { sentence = sentence + 1, word = word - numWords ! paragraph ! sentence }
    | otherwise                                   = Just position

  nextWord :: Position -> Maybe Position
  nextWord position@Position{..}
    | word + 1 < numWords ! paragraph ! sentence  = Just $ position { word = word + 1 }
    | otherwise                                   = nextSentence position

  nextSentence :: Position -> Maybe Position
  nextSentence position@Position{..}
    | sentences + 1 < numSentences ! paragraph    = Just $ position { sentence = sentence + 1, word = 0 }
    -- | sentences + 1 < numSentences ! paragraph    = Just $ position { sentence = sentence + 1, word = word - numWords ! paragraph ! sentence }
    | otherwise                                   = nextParagraph position

  nextParagraph :: Position -> Maybe Position
  nextParagraph position@Position{..}
    | paragraphs + 1 < numParagraphs              = Just $ position { paragraph = paragraph + 1, sentence = 0, word = 0 }
    -- | paragraphs + 1 < numParagraphs              = Just $ position { paragraph = paragraph + 1, sentence = sentence - numSentences ! paragraph }
    | otherwise                                   = Nothing

  previousWord :: Position -> Maybe Position
  previousWord position@Position{..}
    | word > 0        = Just $ position { word = word - 1 }
    | otherwise       = previousSentence position <&> \position@Position{..} -> position { word = numWords ! paragraph ! sentence - 1 }

  previousSentence :: Position -> Maybe Position
  previousSentence position@Position{..}
    | sentences > 0   = Just $ position { sentence = sentence - 1, word = 0 }
    | otherwise       = previousParagraph position <&> \position@Position{..} -> position { sentence = numSentences ! paragraph - 1 } 
    -- | otherwise       = previousParagraph position <&> \position@Position{..} -> position { sentence = numSentences ! paragraph - 1 } 

  previousParagraph :: Position -> Maybe Position
  previousParagraph position@Position{..}
    | paragraphs > 0  = Just $ position { paragraph = paragraph - 1, sentence = 0, word = 0 }
    | otherwise       = Nothing

  numParagraphs = Vector.length document
  numSentences = Vector.length <$> document
  numWords = fmap Vector.length <$> document
  numSentencesPrior = prior numSentences
  numWordsPrior = getCompose . prior $ Compose numWords

numParagraphs :: Document -> Int
numParagraphs d = Vector.length d

numSentences :: Document -> Int -> Int
numSentences d p = Vector.length (d ! p)

numWords :: Document -> Int -> Int -> Int
numWords d p s = Vector.length (d ! p ! s)

numSentencesPrior :: Document -> Int -> Int
numSentencesPrior d p = numSentences d (p - 1)

numWordsPrior :: Document -> Int -> Int -> Int
numWordsPrior d p s = numWords d p (s - 1)

prior :: Traversable t => t Int -> t Int
prior = snd . mapAccumL (\a b -> (b, a)) 0


{-
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
-}
