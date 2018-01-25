module Main where
import qualified Data.Text.IO as Text
import TextIndex

main :: IO ()
main = print . length . findWordTokens =<< Text.readFile "examples/alice.txt"

