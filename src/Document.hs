{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Document
  ( Document
  , parseDocument
  , numParagraphs
  , numSentences
  , numWords
  ) where

import Prelude hiding (Word)
import Data.Char (isSpace)
import Data.List (unfoldr)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint -XOverloadedStrings -XOverloadedLists

type Document = Vector Paragraph
type Paragraph = Vector Sentence
type Sentence = Vector Word
type Word = Text

data WordToken
  = MultipleNewlines
  | SentenceBreak
  | FoundWord Text
  deriving (Show, Eq)

data SentenceToken
  = ParagraphBreak
  | FoundSentence Sentence 
  deriving (Show, Eq)

numParagraphs :: Document -> Int
numParagraphs d = Vector.length d

-- | assumes legal paragraphNum
numSentences :: Document -> Int -> Int
numSentences d paragraphNum = Vector.length (d ! paragraphNum)

-- | assumes legal paragraphNum and sentenceNum
numWords :: Document -> Int -> Int -> Int
numWords d paragraphNum sentenceNum = Vector.length (d ! paragraphNum ! sentenceNum)

-- | terminal characters end sentences
isTerminal :: Char -> Bool
isTerminal ch = ch `elem` (".!?" :: String)

isQuote :: Char -> Bool
isQuote ch = ch `elem` ("\"'" :: String)

-- | 
-- Words are whitespace delimited.
--
--    >>> parseWordTokens "Hello there"
--    [ FoundWord "Hello" , FoundWord "there" ]
--
-- Leading and trailing spaces are ignored.
--
--    >>> parseWordTokens "   Hello there   "
--    [ FoundWord "Hello" , FoundWord "there" ]
--
--  Words can contain arbitrary symbols.
--
--    >>> parseWordTokens "&#$@* shan't"
--    [ FoundWord "&#$@*" , FoundWord "shan't" ]
--
-- Periods, question marks, and exclamation points introduce
-- sentence breaks.
--
--    >>> parseWordTokens "Hello. There."
--    [ FoundWord "Hello."
--    , SentenceBreak
--    , FoundWord "There."
--    , SentenceBreak
--    ]
--
-- Unless there is something other than a quote between
-- them and the end of the word.
--
--    >>> parseWordTokens "'Where is home?'\n127.0.0.1"
--    [ FoundWord "'Where"
--    , FoundWord "is"
--    , FoundWord "home?'"
--    , SentenceBreak
--    , FoundWord "127.0.0.1"
--    ]
--
-- Two or more consecutive newlines also get a token.
--
--    >>> parseWordTokens "\n\n\nHello\n\nthere\n\n\n"
--    [ MultipleNewlines
--    , FoundWord "Hello"
--    , MultipleNewlines
--    , FoundWord "there"
--    , MultipleNewlines
--    ]
--
-- Whitespace is ignored when counting newlines.
--
--    >>> parseWordTokens "Hello   \n\t\r\t  \n   there"
--    [ FoundWord "Hello" , MultipleNewlines , FoundWord "there" ]
parseWordTokens :: Text -> [WordToken]
parseWordTokens = (concat.) . unfoldr $ \text -> if Text.null text then Nothing else
  let (spaces, Text.break isSpace -> (word, next)) = Text.span isSpace text
      numNewlines = Text.length $ Text.filter (=='\n') spaces
      lastNonQuote = fmap snd . Text.unsnoc $ Text.dropWhileEnd isQuote word
      tokens = concat
        [ [ MultipleNewlines | numNewlines >= 2 ]
        , [ FoundWord word | not (Text.null word) ]
        , [ SentenceBreak | maybe False isTerminal lastNonQuote ]
        ]
  in Just (tokens, next)

-- |
-- Sentences are delimited by `SentenceBreak` tokens.
--
--    >>> parseSentenceTokens [FoundWord "abernathy", FoundWord "ate", FoundWord "heartily"]
--    [ FoundSentence [ "abernathy" , "ate" , "heartily" ] ]
--    >>> parseSentenceTokens [FoundWord "Frogs", FoundWord "hop", SentenceBreak, FoundWord "Hi"]
--    [ FoundSentence [ "Frogs" , "hop" ] , FoundSentence [ "Hi" ] ]
--
--  Leading, trailing, or consecutive `SentenceBreak` tokens do not create
--  empty sentences.
--
--    >>> parseSentenceTokens [SentenceBreak, FoundWord "Whee!", SentenceBreak]
--    [ FoundSentence [ "Whee!" ] ]
--    >>> parseSentenceTokens [FoundWord "Hello", SentenceBreak, SentenceBreak, SentenceBreak, FoundWord "there"]
--    [ FoundSentence [ "Hello" ] , FoundSentence [ "there" ] ]
--
-- `MultipleNewlines` tokens also delimit sentences, but are preserved as paragraph breaks.
--
--    >>> parseSentenceTokens [FoundWord "Elbow", FoundWord "macaroni", MultipleNewlines, FoundWord "diesel"]
--    [ FoundSentence [ "Elbow" , "macaroni" ]
--    , ParagraphBreak
--    , FoundSentence [ "diesel" ]
--    ]
parseSentenceTokens :: [WordToken] -> [SentenceToken]
parseSentenceTokens = uncurry cons . foldr step ([], []) where
  step MultipleNewlines (xs, xss) = ([], ParagraphBreak : cons xs xss)
  step SentenceBreak (xs, xss)    = ([],                  cons xs xss)
  step (FoundWord x) (xs, xss)    = (x:xs,                        xss)

  cons [] xss = xss
  cons xs xss = FoundSentence (Vector.fromList xs) : xss
            

-- |
-- Paragraphs are delimited by `ParagraphBreak` tokens.
--
--    >>> parseParagraphs [FoundSentence [ "1" , "2", "3" ], FoundSentence [ "a", "b", "c" ] , FoundSentence [ "i", "ii", "iii" ] ]
--    [ [ [ "1" , "2" , "3" ]
--      , [ "a" , "b" , "c" ]
--      , [ "i" , "ii" , "iii" ]
--      ]
--    ]
--    >>> parseParagraphs [FoundSentence [ "1" , "2", "3" ], ParagraphBreak, FoundSentence [ "a", "b", "c" ] , FoundSentence [ "i", "ii", "iii" ] ]
--    [ [ [ "1" , "2" , "3" ] ]
--    , [ [ "a" , "b" , "c" ] , [ "i" , "ii" , "iii" ] ]
--    ]
--
-- Leading, trailing, and repeated `ParagraphBreak` tokens are ignored.
--
--    >>> parseParagraphs [ParagraphBreak, FoundSentence ["I", "like", "cheese"], ParagraphBreak]
--    [ [ [ "I" , "like" , "cheese" ] ] ]
--    >>> parseParagraphs [FoundSentence ["Fax", "or", "e-mail"], ParagraphBreak, ParagraphBreak, FoundSentence ["Black", "and", "white"]]
--    [ [ [ "Fax" , "or" , "e-mail" ] ]
--    , [ [ "Black" , "and" , "white" ] ]
--    ]
--
parseParagraphs :: [SentenceToken] -> [Paragraph]
parseParagraphs = uncurry cons . foldr step ([], []) where
  step ParagraphBreak (xs, xss)     = ([],  cons xs xss)
  step (FoundSentence x) (xs, xss)  = (x:xs,        xss)

  cons [] xss = xss
  cons xs xss = Vector.fromList xs : xss

-- |
-- Parse a document hierarchically into paragraphs, sentences, and words.
--
--    >>> parseDocument "Hello"
--    [ [ [ "Hello" ] ] ]
--
-- Words are whitespace delimited and may contain arbitrary symbols.
--
--    >>> parseDocument "*Wow*, I'm like :)"
--    [ [ [ "*Wow*," , "I'm" , "like" , ":)" ] ] ]
--
-- Sentences are terminated by words ending in periods, question marks
-- or exclamation marks, and may span multiple lines.
--
--    >>> parseDocument "I lied. \"We want cheese!\" Which dog\n was it?"
--    [ [ [ "I" , "lied." ]
--      , [ "\"We" , "want" , "cheese!\"" ]
--      , [ "Which" , "dog" , "was" , "it?" ]
--      ]
--    ]
--
-- Paragraphs are delimited by two or more newlines.
--
--    >>> parseDocument "See Spot run.\n\nRun, Spot, run!"
--    [ [ [ "See" , "Spot" , "run." ] ]
--    , [ [ "Run," , "Spot," , "run!" ] ]
--    ]
parseDocument :: Text -> Document
parseDocument = Vector.fromList
              . parseParagraphs
              . parseSentenceTokens
              . parseWordTokens
