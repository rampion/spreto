module Heredoc where

getUntil :: String -> IO [String]
getUntil stop = do
  line <- getLine
  if line == stop
      then return []
      else (line:) <$> getUntil stop

-- 
-- >>> :def heredoc \(Prelude.words -> [var, stop]) -> heredoc var stop
-- :def heredoc \(words -> [var, stop]) -> heredoc var stop
--
-- >>> :{
-- ... heredoc "x" "EOF"
-- ... hi
-- ... EOF
-- ... :}
-- "let x = \"hi\n\""
--
heredoc var stop = do
  text <- unlines <$> getUntil stop
  return $ unwords ["let", var, "=", show text]

