module Main where
import Test.DocTest

main :: IO ()
main = doctest $ words "src/Main.hs src/TextIndex.hs"
