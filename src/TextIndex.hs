{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TextIndex
  ( TextIndex(..)
  , textIndex
  , Range
  , SentenceIndex
  , WordIndex
  )
where

import Prelude hiding (words)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings

type Range i = (i,i)
type SentenceIndex = Int
type WordIndex = Int

data TextIndex = TextIndex
  { -- | each paragraph is a half-open range of sentences
    paragraphs  :: !(Vector (Range SentenceIndex))
  , -- | each sentence is a half-open range of words
    sentences   :: !(Vector (Range WordIndex))
  , words       :: !(Vector Text)
  }
  deriving Show

data WordToken
  = MultipleNewlines
  | SentenceBreak
  | Word Text
  deriving (Show, Eq)

-- |
-- Words are whitespace delimited.
--
--    >>> findWordTokens "Hello there"
--    [ Word "Hello" , Word "there" ]
--
-- Leading and trailing spaces are ignored.
--
--    >>> findWordTokens "   Hello there   "
--    [ Word "Hello" , Word "there" ]
--
--  Words can contain arbitrary symbols.
--
--    >>> findWordTokens "&#$@* shan't"
--    [ Word "&#$@*" , Word "shan't" ]
--
-- Periods, question marks, and exclamation points introduce
-- sentence breaks.
--
--    >>> findWordTokens "Hello. There."
--    [ Word "Hello." , SentenceBreak , Word "There." , SentenceBreak ]
--
-- Unless there is something other than a quote between
-- them and the end of the word.
--
--    >>> findWordTokens "'Where is home?'\n127.0.0.1"
--    [ Word "'Where"
--    , Word "is"
--    , Word "home?'"
--    , SentenceBreak
--    , Word "127.0.0.1"
--    ]
--
-- Two or more consecutive newlines also get a token.
--
--    >>> findWordTokens "\n\n\nHello\n\nthere\n\n\n"
--    [ MultipleNewlines
--    , Word "Hello"
--    , MultipleNewlines
--    , Word "there"
--    , MultipleNewlines
--    ]
--
-- Whitespace is ignored when counting newlines.
--
--    >>> findWordTokens "Hello   \n\t\r\t  \n   there"
--    [ Word "Hello" , MultipleNewlines , Word "there" ]
findWordTokens :: Text -> [WordToken]
findWordTokens = findWordStart  where

  findWordStart text 
    | Text.null text    = []
    | numNewlines >= 2  = MultipleNewlines : findWordEnd next
    | otherwise         =                    findWordEnd next
    where (spaces, next) = Text.span isSpace text
          numNewlines = Text.length $ Text.filter (=='\n') spaces

  findWordEnd text
    | Text.null text  = []
    | endOfSentence   = Word word : SentenceBreak : findWordStart next
    | otherwise       = Word word :                 findWordStart next
    where (word, next) = Text.break isSpace text
          endOfSentence = maybe False (isTerminal . snd) . Text.unsnoc $ Text.dropWhileEnd isQuote word
          
  
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
--    >>> findSentenceTokens [Word "abernathy", Word "ate", Word "heartily"]
--    [ Sentence ( 0 , 3 ) ]
--    >>> findSentenceTokens [Word "Frogs", Word "hop", SentenceBreak, Word "Hi"]
--    [ Sentence ( 0 , 2 ) , Sentence ( 2 , 3 ) ]
--
--  Leading, trailing, or consecutive `SentenceBreak` tokens do not create
--  empty sentences.
--
--    >>> findSentenceTokens [SentenceBreak, Word "Whee!", SentenceBreak]
--    [ Sentence ( 0 , 1 ) ]
--    >>> findSentenceTokens [Word "Hello", SentenceBreak, SentenceBreak, SentenceBreak, Word "there"]
--    [ Sentence ( 0 , 1 ) , Sentence ( 1 , 2 ) ]
--
-- `MultipleNewlines` tokens also delimit sentence, but are preserved as paragraph breaks.
--
--    >>> findSentenceTokens [Word "Elbow", Word "macaroni", MultipleNewlines, Word "diesel"]
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
--      , words = [ "Hello" ]
--      }
--
-- Words are whitespace delimited and may contain arbitrary symbols.
--
--    >>> textIndex "*Wow*, I'm like :)"
--    TextIndex
--      { paragraphs = [ ( 0 , 1 ) ]
--      , sentences = [ ( 0 , 4 ) ]
--      , words = [ "*Wow*," , "I'm" , "like" , ":)" ]
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
