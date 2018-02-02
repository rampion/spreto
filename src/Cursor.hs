{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cursor 
  ( Cursor(), cursor
  , getDocument, getPosition, getWord
  , Increment(..), move
  )
  where
import Control.Monad (guard, (>=>))

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!))

import Position
import Document

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

data Cursor = Cursor
  { getDocument :: Document
  , getPosition :: Position
  }

getWord :: Cursor -> Text
getWord (Cursor d Position{..}) = d ! paragraphNum ! sentenceNum ! wordNum

-- |
-- >>> Cursor oz (Position 0 0 0)
-- 0.0.0:Suddenly
-- >>> Cursor oz (Position 1 2 3)
-- 1.2.3:toward
-- >>> Cursor oz (Position 3 2 3)
-- 3.2.3:cellar!"
instance Show Cursor where
  showsPrec _ c 
    = shows (getPosition c)
    . showString ":"
    . showString (Text.unpack $ getWord c)

-- | Smart constructor for cursor
--
-- >>> cursor oz (Position 1 (-1) 0)
-- Just 0.0.0:Suddenly
-- >>> cursor oz (Position 0 0 24)
-- Just 1.2.3:toward
-- >>> cursor oz (Position 0 8 3)
-- Just 3.2.3:cellar!"
cursor :: Document -> Position -> Maybe Cursor
cursor d = fmap (Cursor d) <$> \p -> do
  guard $ 0 <= paragraphNum p && paragraphNum p < numParagraphs d

  let makeLegalSentenceNum 
        | sentenceNum p < 0 = 
          paragraphToSentences d >=> \p ->
          if sentenceNum p < 0 
            then makeLegalSentenceNum p
            else return p
        | sentenceNumInNextParagraph d p >= 0 =
          sentencesToParagraph d >=> \p ->
          if sentenceNumInNextParagraph d p >= 0
            then makeLegalSentenceNum p
            else return p
        | otherwise = return

  p <- makeLegalSentenceNum p

  let makeLegalWordNum
        | wordNum p < 0 =
          sentenceToWords d >=> \p ->
          if wordNum p < 0
            then makeLegalWordNum p
            else return p
        | wordNumInNextSentence d p >= 0 =
          wordsToSentence d >=> \p ->
          if wordNumInNextSentence d p >= 0
            then makeLegalWordNum p
            else return p
        | otherwise = return

  makeLegalWordNum p

--------------------------------------------------------------------------------

data Increment
  = ToNextWord
  | ToPreviousWord
  | ToNextSentence
  | ToPreviousSentence
  | ToNextParagraph
  | ToPreviousParagraph 

-- | Try to move a cursor by a given increment
--
-- >>> cursor oz $ Position 0 0 0
-- Just 0.0.0:Suddenly
-- >>> move ToNextWord =<< it
-- Just 0.0.1:Uncle
-- >>> move ToNextWord =<< it
-- Just 0.0.2:Henry
-- >>> move ToNextSentence =<< it
-- Just 1.0.0:"There's
-- >>> move ToNextSentence =<< it
-- Just 1.1.0:"I'll
-- >>> move ToNextParagraph =<< it
-- Just 2.0.0:Aunt
-- >>> move ToNextParagraph =<< it
-- Just 3.0.0:"Quick,
-- >>> move ToNextParagraph =<< it
-- Nothing
move :: Increment -> Cursor -> Maybe Cursor
move ToNextWord (Cursor d p) = Cursor d <$>
  let q = p { wordNum = wordNum p + 1 } in
  (if wordNumInNextSentence d q >= 0 then wordsToSentence d else Just) q
move ToPreviousWord (Cursor d p) = Cursor d <$>
  let q = p { wordNum = wordNum p - 1 } in
  (if wordNum q <= 0 then sentenceToWords d else Just) q
move ToNextSentence (Cursor d p) = Cursor d <$>
  let q = p { sentenceNum = sentenceNum p + 1, wordNum = 0 } in
  (if sentenceNumInNextParagraph d q >= 0 then sentencesToParagraph d else Just) q
move ToPreviousSentence (Cursor d p) = Cursor d <$>
  let q = p { sentenceNum = sentenceNum p - 1, wordNum = 0 } in
  (if sentenceNum q <= 0 then paragraphToSentences d else Just) q
move ToNextParagraph (Cursor d p) = Cursor d <$>
  let q = Position { paragraphNum = paragraphNum p + 1, sentenceNum = 0, wordNum = 0 } in
  (if paragraphNum q >= numParagraphs d then const Nothing else Just) q
move ToPreviousParagraph (Cursor d p) = Cursor d <$>
  let q = Position { paragraphNum = paragraphNum p - 1, sentenceNum = 0, wordNum = 0 } in
  (if paragraphNum q < 0 then const Nothing else Just) q

--------------------------------------------------------------------------------

-- | Exchange sentences for a paragraph
--
-- Given 0 <= paragraphNum, preserves legality of paragraphNum in result
sentencesToParagraph :: Document -> Position -> Maybe Position
sentencesToParagraph d p@Position{..} = do
  guard $ paragraphNum + 1 < numParagraphs d 
  return p { paragraphNum = paragraphNum + 1, sentenceNum = sentenceNumInNextParagraph d p }

-- | Exchange a paragraph for sentences
--
-- Given paragraphNum < numParagraphs d, preserves legality of paragraphNum in result
paragraphToSentences :: Document -> Position -> Maybe Position
paragraphToSentences d p@Position{..} = do
  guard $ 0 < paragraphNum
  return p { paragraphNum = paragraphNum - 1, sentenceNum = sentenceNumInPreviousParagraph d p }

-- | Exchange words for a sentence
--
-- Given 0 <= paragraphNum < numParagraphs d and 0 <= sentenceNum, 
-- preserves legality of both in result
wordsToSentence :: Document -> Position -> Maybe Position
wordsToSentence d p =
  let q = p { sentenceNum = sentenceNum p + 1, wordNum = wordNumInNextSentence d p } in
  (if sentenceNumInNextParagraph d q >= 0 then sentencesToParagraph d else Just) q

-- | Exchange a sentence for words
--
-- Given 0 <= paragraphNum < numParagraphs d and sentenceNum < numSentences d paragraphNum, 
-- preserves legality of both in result
sentenceToWords :: Document -> Position -> Maybe Position
sentenceToWords d p =
  let q = p { sentenceNum = sentenceNum p - 1, wordNum = wordNumInPreviousSentence d p } in
  (if sentenceNum q < 0 then paragraphToSentences d else Just) q

-- | What sentenceNum would point to the focused sentence if the cursor moved
-- to the next paragraph.
--
-- Assumes paragraphNum is legal
sentenceNumInNextParagraph :: Document -> Position -> Int
sentenceNumInNextParagraph d Position{..} = sentenceNum - numSentences d paragraphNum

-- | What sentenceNum would point to the focused sentence if the cursor moved
-- to the previous paragraph.
--
-- Assumes paragraphNum is legal
sentenceNumInPreviousParagraph :: Document -> Position -> Int
sentenceNumInPreviousParagraph d Position{..} = sentenceNum + numSentences d (paragraphNum - 1)

-- | What wordNum would point the focused word if the cursor moved to the next
-- paragraph.
--
-- Assumes paragraphNum and sentenceNum are legal
wordNumInNextSentence :: Document -> Position -> Int
wordNumInNextSentence d Position{..} = wordNum - numWords d paragraphNum sentenceNum

-- | What wordNum would point to the focused word if the cursor moved to the
-- previous paragraph.
--
-- Assumes paragraphNum and sentenceNum are legal
wordNumInPreviousSentence :: Document -> Position -> Int
wordNumInPreviousSentence d Position{..} = wordNum + numWords d paragraphNum (sentenceNum - 1)
