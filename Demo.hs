{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO
import Control.Monad
import Control.Concurrent
import System.Clock

timeSpecToMicroseconds :: TimeSpec -> Double
timeSpecToMicroseconds TimeSpec{..} = 1000*1000*fromIntegral sec + fromIntegral nsec / 1000

main :: IO ()
main = do
  putStr "\ESC[?25l"
  putStrLn "────────────────────┬───────────────────────────────────────────────────────────"
  putStrLn ""
  putStrLn "────────────────────┴───────────────────────────────────────────────────────────\ESC[F"
  let n = lcm (59*8) (20*3)
      lefts  = "" : map return "▏▎▍▌▋▊▉" 
      rights = "" : map return "▕▐"

      -- frames per microsecond
      fpus = (fromIntegral n + 1) / 5e6

  startTime <- timeSpecToMicroseconds <$> getTime Monotonic

  forM_ [n,n-1..0] $ \i -> do
    let (rfull,rpart) = (20 * i) `quotRem` n
        (lfull,lpart) = (59 * i) `quotRem` n
        full = rfull + lfull + 1
        prefix = rights !! quot (3 * rpart) n
        suffix = lefts  !! quot (8 * lpart) n
        indent = 20 - rfull - if null prefix then 0 else 1
    putStrLn $ concat
      [ "\ESC[F\ESC[K\ESC[", show indent, "C"
      , prefix, replicate full '█', suffix
      ]

    -- smooth out timing from threadDelay so the entire animation
    -- takes no longer than desired
    --
    -- probably undesirable in word presentation
    currTime <- timeSpecToMicroseconds <$> getTime Monotonic
    let frameNum = fromIntegral $ n - i
    let delay = round $ startTime + (frameNum + 1)/fpus - currTime
    threadDelay delay

  putStrLn "\ESC[?25h"
  endTime <- timeSpecToMicroseconds <$> getTime Monotonic
  print $ (round $ endTime - startTime :: Integer)
