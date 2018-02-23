{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ORP where
import Data.Text (Text)
import qualified Data.Text as Text
import Brick.Widgets.Core (textWidth)
import Graphics.Text.Width (wcwidth)

import Cursor

-- $setup
-- >>> :set -XOverloadedStrings

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
orp :: Text -> Int
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
orp w = (textWidth w + 2) `div` 4

-- |
-- Split text at the n'th column.
--
-- >>> putStrLn "|123456|\n|\65345\65346\65347|"
-- |123456|
-- |ａｂｃ|
-- >>> splitWidth 0 "\65345\65346\65347"
-- ("","\65345\65346\65347")
-- >>> splitWidth 2 "\65345\65346\65347"
-- ("\65345","\65346\65347")
-- >>> splitWidth 4 "\65345\65346\65347"
-- ("\65345\65346","\65347")
-- >>> splitWidth 6 "\65345\65346\65347"
-- ("\65345\65346\65347","")
--
-- If the n'th column occurs in the middle of a character, the text is split
-- right before that character.
--
-- >>> splitWidth 1 "\65345\65346\65347"
-- ("","\65345\65346\65347")
-- >>> splitWidth 3 "\65345\65346\65347"
-- ("\65345","\65346\65347")
-- >>> splitWidth 5 "\65345\65346\65347"
-- ("\65345\65346","\65347")
splitWidth :: Int -> Text -> (Text, Text)
splitWidth n t = case Text.uncons t of
  Just (c, t) | wcwidth c <= n -> 
    let ~(xs,ys) = splitWidth (n - wcwidth c) t
    in (Text.cons c xs, ys)
  _ -> (Text.empty, t)

-- |
-- Split text at the n'th column from the end.
--
-- >>> putStrLn "|123456|\n|\65345\65346\65347|"
-- |123456|
-- |ａｂｃ|
-- >>> rsplitWidth 0 "\65345\65346\65347"
-- ("\65345\65346\65347","")
-- >>> rsplitWidth 2 "\65345\65346\65347"
-- ("\65345\65346","\65347")
-- >>> rsplitWidth 4 "\65345\65346\65347"
-- ("\65345","\65346\65347")
-- >>> rsplitWidth 6 "\65345\65346\65347"
-- ("","\65345\65346\65347")
--
-- If the n'th column from the end occurs in the middle of a character, the string is split
-- right after that character.
--
-- >>> rsplitWidth 1 "\65345\65346\65347"
-- ("\65345\65346\65347","")
-- >>> rsplitWidth 3 "\65345\65346\65347"
-- ("\65345\65346","\65347")
-- >>> rsplitWidth 5 "\65345\65346\65347"
-- ("\65345","\65346\65347")
rsplitWidth :: Int -> Text -> (Text, Text)
rsplitWidth n t = case Text.unsnoc t of
  Just (t, c) | wcwidth c <= n -> 
    let ~(xs,ys) = rsplitWidth (n - wcwidth c) t
    in (xs, Text.snoc ys c)
  _ -> (t, Text.empty)

-- |
-- Split the given word around the optimal recognition point
--
-- >>> splitORP "abcde"
-- ("a",'b',"cde")
--
-- The ORP is calculated using number of columns each character takes up:
--
-- >>> splitORP "aabbCDE"
-- ("aa",'b',"bCDE")
-- >>> splitORP "aabb\65347\65348\65349"
-- ("aab",'b',"\65347\65348\65349")
splitORP :: Text -> (Text, Char, Text)
splitORP word = (pre, c, suf) where
  (pre,Just (c, suf)) = Text.uncons <$> splitWidth (orp word) word
