{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TextIndex
{-
  ( TextIndex(..)
  , textIndex
  , Range
  , ParaIndex
  , SentenceIndex
  , WordIndex
  , CharacterIndex
  )
  -}
where

import Prelude hiding (words)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings -Wno-missing-fields

type Range i = (i,i)
type ParaIndex = Int
type SentenceIndex = Int
type WordIndex = Int
type CharacterIndex = Int

data TextIndex = TextIndex
  { -- | each paragraph is a half-open range of sentences
    paragraphs  :: (Vector (Range SentenceIndex))
  , -- | each sentence is a half-open range of words
    sentences   :: (Vector (Range WordIndex))
  , -- | each word is a half-open range of characters
    words       :: (Vector (Range CharacterIndex))
  , -- | the source material
    characters  :: Text
  }
  deriving Show

data WordToken
  = MultipleNewlines
  | SentenceBreak
  | Word (Range CharacterIndex)
  deriving (Show, Eq)

-- |
-- Words are whitespace delimited.
--
--    >>> findWordTokens "Hello there"
--    [ Word ( 0 , 5 ) , Word ( 6 , 11 ) ]
--
-- Leading and trailing spaces are ignored.
--
--    >>> findWordTokens "   Hello there   "
--    [ Word ( 3 , 8 ) , Word ( 9 , 14 ) ]
--
--  Words can contain arbitrary symbols.
--
--    >>> findWordTokens "&#$@* shan't"
--    [ Word ( 0 , 5 ) , Word ( 6 , 12 ) ]
--
-- Periods, question marks, and exclamation points introduce
-- sentence breaks.
--
--    >>> findWordTokens "Hello. There."
--    [ Word ( 0 , 6 )
--    , SentenceBreak
--    , Word ( 7 , 13 )
--    , SentenceBreak
--    ]
--
-- Unless there is something other than a quote between
-- them and the end of the word.
--
--    >>> findWordTokens "'Where is home?'\n127.0.0.1"
--    [ Word ( 0 , 6 )
--    , Word ( 7 , 9 )
--    , Word ( 10 , 16 )
--    , SentenceBreak
--    , Word ( 17 , 26 )
--    ]
--
-- Two or more consecutive newlines also get a token.
--
--    >>> findWordTokens "\n\n\nHello\n\nthere\n\n\n"
--    [ MultipleNewlines
--    , Word ( 3 , 8 )
--    , MultipleNewlines
--    , Word ( 10 , 15 )
--    , MultipleNewlines
--    ]
--
-- Whitespace is ignored when counting newlines.
--
--    >>> findWordTokens "Hello   \n\t\r\t  \n   there"
--    [ Word ( 0 , 5 ) , MultipleNewlines , Word ( 18 , 23 ) ]
findWordTokens :: Text -> [WordToken]
findWordTokens = findWordStart 0 0 . Text.unpack where

  findWordStart _ n []    = [ MultipleNewlines | n >= 2 ]
  findWordStart i n (ch:ct)
    | isSpace ch          = findWordStart (i + 1) (n + fromEnum (ch == '\n')) ct
    | otherwise           = [ MultipleNewlines | n >= 2 ] ++ findWordEnd i (i + 1) False ct

  findWordEnd i j b []    = Word (i, j) : [ SentenceBreak | b ]
  findWordEnd i j b (ch:ct)
    | isSpace ch          = Word (i, j) : [ SentenceBreak | b ] ++ findWordStart (j + 1) (fromEnum $ ch == '\n') ct
    | otherwise           = findWordEnd i (j + 1) (isTerminal ch || (b && isQuote ch)) ct

isTerminal :: Char -> Bool
isTerminal ch = ch `elem` (".!?" :: String)

isQuote :: Char -> Bool
isQuote ch = ch `elem` ("\"'" :: String)

data SentenceToken
  = ParagraphBreak
  | Sentence (Range WordIndex)
  deriving (Show, Eq)

-- |
-- Sentences are delimited by `SentenceBreak` tokens.
--
--    >>> findSentenceTokens [Word (0,1), Word (2,3), Word (4,5)]
--    [ Sentence ( 0 , 3 ) ]
--    >>> findSentenceTokens [Word (0,1), Word (2,3), SentenceBreak, Word (4,5)]
--    [ Sentence ( 0 , 2 ) , Sentence ( 2 , 3 ) ]
--
--  Leading, trailing, or consecutive `SentenceBreak` tokens do not create
--  empty sentences.
--
--    >>> findSentenceTokens [SentenceBreak, Word (0,1), SentenceBreak]
--    [ Sentence ( 0 , 1 ) ]
--    >>> findSentenceTokens [Word (0,1), SentenceBreak, SentenceBreak, SentenceBreak, Word (4,5)]
--    [ Sentence ( 0 , 1 ) , Sentence ( 1 , 2 ) ]
--
-- `MultipleNewlines` tokens also delimit sentence, but are preserved as paragraph breaks.
--
--    >>> findSentenceTokens [Word (0,1), Word (2,3), MultipleNewlines, Word (4,5)]
--    [ Sentence ( 0 , 2 ) , ParagraphBreak , Sentence ( 2 , 3 ) ]
findSentenceTokens :: [WordToken] -> [SentenceToken]
findSentenceTokens = findSentenceStart 0 where

  findSentenceStart _ [] = []
  findSentenceStart i (t:ts) = case t of
    Word _  -> findSentenceEnd i (i + 1) ts
    _       -> [ ParagraphBreak | t == MultipleNewlines ] ++ findSentenceStart i ts

  findSentenceEnd i j [] = [Sentence (i, j)]
  findSentenceEnd i j (t:ts) = case t of
    Word _  -> findSentenceEnd i (j + 1) ts
    _       -> Sentence (i, j) : [ ParagraphBreak | t == MultipleNewlines ] ++ findSentenceStart j ts

-- |
-- Paragraphs are delimited by `ParagraphBreak` tokens.
--
--    >>> findParagraphs [Sentence (0,2), Sentence (2,4), Sentence (5,7)]
--    [ ( 0 , 3 ) ]
--    >>> findParagraphs [Sentence (0,2), ParagraphBreak, Sentence (2,4), Sentence (5,7)]
--    [ ( 0 , 1 ) , ( 1 , 3 ) ]
--
-- Leading, trailing, and repeated `ParagraphBreak` tokens are ignored.
--
--    >>> findParagraphs [ParagraphBreak, Sentence (0,2), ParagraphBreak]
--    [ ( 0 , 1 ) ]
--    >>> findParagraphs [Sentence (0,2), ParagraphBreak, ParagraphBreak, Sentence (2, 4)]
--    [ ( 0 , 1 ) , ( 1 , 2 ) ]
--
findParagraphs :: [SentenceToken] -> [Range SentenceIndex]
findParagraphs = findParagraphStart 0 where

  findParagraphStart _ [] = []
  findParagraphStart i (t:ts) = case t of
    Sentence _  -> findParagraphEnd i (i + 1) ts
    _           -> findParagraphStart i ts

  findParagraphEnd i j [] = [(i, j)]
  findParagraphEnd i j (t:ts) = case t of
    Sentence _  -> findParagraphEnd i (j + 1) ts
    _           -> (i,j) : findParagraphStart j ts


-- |
-- 'textIndex' finds the location of each word, sentence, and paragraph in the
-- given text.
--
--    >>> textIndex "Hello"
--    TextIndex
--      { paragraphs = [ ( 0 , 1 ) ]
--      , sentences = [ ( 0 , 1 ) ]
--      , words = [ ( 0 , 5 ) ]
--      , characters = "Hello"
--      }
--
-- Words are whitespace delimited and may contain arbitrary symbols.
--
--    >>> textIndex "*Wow*, I'm like :)"
--    TextIndex
--      { paragraphs = [ ( 0 , 1 ) ]
--      , sentences = [ ( 0 , 4 ) ]
--      , words = [ ( 0 , 6 ) , ( 7 , 10 ) , ( 11 , 15 ) , ( 16 , 18 ) ]
--    ...
--      }
--
-- Sentences are terminated by words ending in periods, question marks
-- or exclamation marks, and may span multiple lines.
--
--    >>> textIndex "I lied. \"We want cheese!\" Which dog\n was it?"
--    TextIndex
--      { paragraphs = [ ( 0 , 3 ) ]
--      , sentences = [ ( 0 , 2 ) , ( 2 , 5 ) , ( 5 , 9 ) ]
--    ...
--      }
--
-- Paragraphs are delimited by two or more newlines.
--
--    >>> textIndex "1.\n2.\n3.\n\na? b!\nc? d!"
--    TextIndex
--      { paragraphs = [ ( 0 , 3 ) , ( 3 , 7 ) ]
--    ...
--      }
textIndex :: Text -> TextIndex
textIndex characters = TextIndex{..} where
  paragraphs      = Vector.fromList $ findParagraphs sentenceTokens
  sentences       = Vector.fromList [ r | Sentence r <- sentenceTokens ]
  words           = Vector.fromList [ r | Word r <- wordTokens ]
  sentenceTokens  = findSentenceTokens wordTokens
  wordTokens      = findWordTokens characters
