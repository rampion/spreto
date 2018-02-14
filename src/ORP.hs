module ORP where
import Data.Text (Text)
import qualified Data.Text as Text

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
orp w = (Text.length w + 2) `div` 4

-- TODO: Switch to using Brick.Widgets.Core.textWidth :: Text -> Int
-- or Graphics.Text.Width.wcswidth :: String -> Int
-- or Graphics.Text.Width.wcwidth :: Char -> Int
