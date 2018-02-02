{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Position where

-- | Offset in a given text
data Position = Position
  { paragraphNum :: Int
  , sentenceNum :: Int
  , wordNum :: Int
  }

-- |
-- >>> Position 409 0 1
-- 409.0.1
instance Show Position where
  showsPrec _ Position{..}
    = shows paragraphNum . showString "."
    . shows sentenceNum . showString "."
    . shows wordNum

-- |
-- >>> paragraphNum $ read "409.0.1"
-- 409
-- >>> sentenceNum $ read "409.0.1"
-- 0
-- >>> wordNum $ read "409.0.1"
-- 1
instance Read Position where
  readsPrec = const go where
    go ( splitDot -> (reads -> [(paragraphNum, "")]
       , splitDot -> (reads -> [(sentenceNum, "")]
       ,              reads -> [(wordNum, "")])))
      = [(Position{..}, "")]
    go _ = []

    splitDot = fmap tail <$> break ('.'==)
