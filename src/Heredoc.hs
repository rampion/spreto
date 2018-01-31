module Heredoc where
import Data.List (intercalate)
import Data.Char (isUpper)

whileM :: Monad m => (a -> Bool) -> m a -> m [a]
whileM p m = m >>= \a -> case p a of
  True  -> (a:) <$> whileM p m
  False -> return []

heredocCmd :: String -> IO String
heredocCmd ('<':'<':cs) = case span isUpper cs of 
  ([], _) -> ('<':) <$> heredocCmd ('<':cs)
  (n, ct) -> do
    t <- intercalate "\n" <$> whileM (/= n) getLine
    (show t ++) <$> heredocCmd ct
heredocCmd (c:cs) = (c:) <$> heredocCmd cs
heredocCmd [] = return []
