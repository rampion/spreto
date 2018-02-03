{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cursor 
  ( Cursor(), cursor
  , getDocument, getPosition, getWord
  , Increment(..), move
  )
  where
import Data.Function (fix)
import Control.Monad (guard, (>=>), (<=<))

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!))

import Position
import Document

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> import Data.Text.IO as TextIO
-- >>> import qualified Data.Vector as Vector
-- >>> import Text.Wrap (wrapText, defaultWrapSettings)
-- >>> :set -interactive-print pPrint -XOverloadedStrings
-- >>> wrapParagraph = wrapText defaultWrapSettings 80 . Text.unwords . Vector.toList . Vector.concat . Vector.toList
-- >>> putParagraphs = TextIO.putStrLn . Text.intercalate "\n\n" . map wrapParagraph


data Cursor = Cursor
  { getDocument :: Document
  , getPosition :: Position
  }

getWord :: Cursor -> Text
getWord (Cursor d Position{..}) = d ! paragraphNum ! sentenceNum ! wordNum

-- |
-- Shows the position and word that the cursor is currently focused on.
--
-- >>> oz <- parseDocument <$> TextIO.readFile "examples/oz.txt"
-- >>> putParagraphs [oz ! i | i <- [26..29]]
-- Suddenly Uncle Henry stood up.
-- <BLANKLINE>
-- "There's a cyclone coming, Em," he called to his wife. "I'll go look after the
-- stock." Then he ran toward the sheds where the cows and horses were kept.
-- <BLANKLINE>
-- Aunt Em dropped her work and came to the door. One glance told her of the danger
-- close at hand.
-- <BLANKLINE>
-- "Quick, Dorothy!" she screamed. "Run for the cellar!"
-- >>> Cursor oz (Position 26 0 0)
-- 26.0.0:Suddenly
-- >>> Cursor oz (Position 27 2 3)
-- 27.2.3:toward
-- >>> Cursor oz (Position 29 2 3)
-- 29.2.3:cellar!"
instance Show Cursor where
  showsPrec _ c 
    = shows (getPosition c)
    . showString ":"
    . showString (Text.unpack $ getWord c)

-- | Smart constructor for 'Cursor'
--
-- >>> oz <- parseDocument <$> TextIO.readFile "examples/oz.txt"
-- >>> putParagraphs [oz ! i | i <- [26..29]]
-- Suddenly Uncle Henry stood up.
-- <BLANKLINE>
-- "There's a cyclone coming, Em," he called to his wife. "I'll go look after the
-- stock." Then he ran toward the sheds where the cows and horses were kept.
-- <BLANKLINE>
-- Aunt Em dropped her work and came to the door. One glance told her of the danger
-- close at hand.
-- <BLANKLINE>
-- "Quick, Dorothy!" she screamed. "Run for the cellar!"
--
-- Legal positions are left unchanged.
--
-- >>> cursor oz (Position 26 0 0)
-- Just 26.0.0:Suddenly
-- >>> cursor oz (Position 27 2 3)
-- Just 27.2.3:toward
-- >>> cursor oz (Position 29 2 3)
-- Just 29.2.3:cellar!"
--
-- Negative address components are used for reverse indexing
-- with the current document/paragraph/sentence.
--
-- >>> cursor oz (Position (-1) (-1) (-1))
-- Just 1198.0.39:eBooks.
-- >>> cursor oz (Position 29 (-1) (-1))
-- Just 29.2.3:cellar!"
--
-- Addressing a word or sentence beyond the end (beginning) of a sentence or
-- paragraph moves into the next (previous) sentence or paragraph.
--
-- >>> cursor oz (Position 26 0 24)
-- Just 27.2.3:toward
-- >>> cursor oz (Position 27 2 (-34))
-- Just 26.0.0:Suddenly
--
-- Addressing beyond the limits of the document fails.
--
-- >>> cursor oz (Position 1198 0 40)
-- Nothing
-- >>> cursor oz (Position (-1199) (-2) (-13))
-- Nothing
cursor :: Document -> Position -> Maybe Cursor
cursor d = fmap (Cursor d) . normWordNum <=< normSentenceNum <=< normParagraphNum where
  normParagraphNum p@Position{..}
    | paragraphNum < -n = Nothing
    | paragraphNum < 0  = Just p { paragraphNum = paragraphNum + n }
    | paragraphNum < n  = Just p
    | otherwise         = Nothing
    where n = numParagraphs d

  normSentenceNum p@Position{..}
    | sentenceNum < 0   = flip fix p { sentenceNum = sentenceNum + n } $ \loop p@Position{..} ->
                            if sentenceNum < 0
                              then loop =<< paragraphToSentences d p
                              else Just p
    | sentenceNum >= n  = flip fix p $ \loop -> sentencesToParagraph d >=> \p@Position{..} ->
                            if sentenceNum >= numSentences d paragraphNum
                              then loop p
                              else Just p
    | otherwise         = Just p
    where n = numSentences d paragraphNum

  normWordNum p@Position{..}
    | wordNum < 0   = flip fix p { wordNum = wordNum + n } $ \loop p@Position{..} ->
                        if wordNum < 0
                          then loop =<< sentenceToWords d p
                          else Just p
    | wordNum >= n  = flip fix p $ \loop -> wordsToSentence d >=> \p@Position{..} ->
                        if wordNum >= numWords d paragraphNum sentenceNum
                          then loop p
                          else Just p
    | otherwise     = Just p
    where n = numWords d paragraphNum sentenceNum

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
-- >>> oz <- parseDocument <$> TextIO.readFile "examples/oz.txt"
-- >>> putParagraphs [oz ! i | i <- [26..29]]
-- Suddenly Uncle Henry stood up.
-- <BLANKLINE>
-- "There's a cyclone coming, Em," he called to his wife. "I'll go look after the
-- stock." Then he ran toward the sheds where the cows and horses were kept.
-- <BLANKLINE>
-- Aunt Em dropped her work and came to the door. One glance told her of the danger
-- close at hand.
-- <BLANKLINE>
-- "Quick, Dorothy!" she screamed. "Run for the cellar!"
-- >>> cursor oz $ Position 26 0 0
-- Just 26.0.0:Suddenly
-- >>> move ToNextWord =<< it
-- Just 26.0.1:Uncle
-- >>> move ToNextWord =<< it
-- Just 26.0.2:Henry
-- >>> move ToNextSentence =<< it
-- Just 27.0.0:"There's
-- >>> move ToNextSentence =<< it
-- Just 27.1.0:"I'll
-- >>> move ToNextParagraph =<< it
-- Just 28.0.0:Aunt
-- >>> move ToNextParagraph =<< it
-- Just 29.0.0:"Quick,
-- >>> cursor oz (Position 1198 0 39)
-- Just 1198.0.39:eBooks.
-- >>> move ToNextParagraph =<< it
-- Nothing
move :: Increment -> Cursor -> Maybe Cursor
move i (Cursor d p@Position{..}) = Cursor d <$> case i of
  ToNextWord -> 
    p { wordNum = wordNum + 1 } &
    if wordNum + 1 >= numWords d paragraphNum sentenceNum
      then wordsToSentence d
      else Just
  ToPreviousWord ->
    p { wordNum = wordNum - 1 } &
    if wordNum <= 0
      then sentenceToWords d
      else Just
  ToNextSentence ->
    p { sentenceNum = sentenceNum + 1, wordNum = 0 } &
    if sentenceNum + 1 >= numSentences d paragraphNum
      then sentencesToParagraph d
      else Just
  ToPreviousSentence ->
    p { sentenceNum = sentenceNum - 1, wordNum = 0 } &
    if sentenceNum <= 0
      then paragraphToSentences d
      else Just
  ToNextParagraph ->
    Position { paragraphNum = paragraphNum + 1, sentenceNum = 0, wordNum = 0 } &
    if paragraphNum + 1 >= numParagraphs d
      then const Nothing
      else Just
  ToPreviousParagraph ->
    Position { paragraphNum = paragraphNum - 1, sentenceNum = 0, wordNum = 0 } &
    if paragraphNum <= 0
      then const Nothing
      else Just

--------------------------------------------------------------------------------

-- | Exchange sentences for a paragraph
--
-- Given 0 <= paragraphNum, preserves legality of paragraphNum in result
sentencesToParagraph :: Document -> Position -> Maybe Position
sentencesToParagraph d p@Position{..} = do
  guard $ paragraphNum + 1 < numParagraphs d 
  return p { paragraphNum = paragraphNum + 1, sentenceNum = sentenceNum - numSentences d paragraphNum }

-- | Exchange a paragraph for sentences
--
-- Given paragraphNum < numParagraphs d, preserves legality of paragraphNum in result
paragraphToSentences :: Document -> Position -> Maybe Position
paragraphToSentences d p@Position{..} = do
  guard $ 0 < paragraphNum
  return p { paragraphNum = paragraphNum - 1, sentenceNum = sentenceNum + numSentences d (paragraphNum - 1) }

-- | Exchange words for a sentence
--
-- Given 0 <= paragraphNum < numParagraphs d and 0 <= sentenceNum, 
-- preserves legality of both in result
wordsToSentence :: Document -> Position -> Maybe Position
wordsToSentence d p@Position{..} =
  p { sentenceNum = sentenceNum + 1, wordNum = wordNum - numWords d paragraphNum sentenceNum } &
  if sentenceNum + 1 >= numSentences d paragraphNum
    then sentencesToParagraph d
    else Just

-- | Exchange a sentence for words
--
-- Given 0 <= paragraphNum < numParagraphs d and sentenceNum < numSentences d paragraphNum, 
-- preserves legality of both in result
sentenceToWords :: Document -> Position -> Maybe Position
sentenceToWords d p@Position{..} = do
  p@Position{..} <- p { sentenceNum = sentenceNum - 1 } & if sentenceNum - 1 < 0
    then paragraphToSentences d
    else Just
  return p { wordNum = wordNum + numWords d paragraphNum sentenceNum }

(&) :: a -> (a -> b) -> b
a & f = f a
