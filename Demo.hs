{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO
import Control.Monad
import Control.Concurrent
import System.Clock
import Data.Vector ((!))
import qualified Data.Vector as Vector

timeSpecToMicroseconds :: TimeSpec -> Double
timeSpecToMicroseconds TimeSpec{..} = 1000*1000*fromIntegral sec + fromIntegral nsec / 1000

main :: IO ()
main = do
  let leftEdges   = Vector.fromList $ "" : map return "▕▐" 
      rightEdges  = Vector.fromList $ "" : map return "▏▎▍▌▋▊▉"
      prefixWidth = 20
      suffixWidth = 59
      durationInMicroseconds = 5e6

      numLeftEdges = Vector.length leftEdges
      numRightEdges = Vector.length rightEdges

      numFrames = lcm (prefixWidth * numLeftEdges)
                      (suffixWidth * numRightEdges)

      framesPerMicrosecond = fromIntegral numFrames  / durationInMicroseconds

      reticule c = replicate prefixWidth '─' ++ c : replicate suffixWidth '─'

  putStr "\ESC[?25l" -- hide cursor
  putStrLn $ reticule '┬'
  putStrLn ""
  putStrLn $ reticule '┴'
  putStr "\ESC[F" -- move up one line

  startTime <- timeSpecToMicroseconds <$> getTime Monotonic

  forM_ [0..numFrames] $ \frameNum -> do
    let framesRemaining = numFrames - frameNum
        (leftFull, leftPartial)   = (prefixWidth * framesRemaining) `quotRem` numFrames
        (rightFull, rightPartial) = (suffixWidth * framesRemaining) `quotRem` numFrames
        numFull = leftFull + 1 + rightFull
        leftEdge  = leftEdges ! quot (numLeftEdges * leftPartial) numFrames
        rightEdge = rightEdges ! quot (numRightEdges * rightPartial) numFrames
        indent = prefixWidth - leftFull - length leftEdge

    putStrLn $ concat
      [ "\ESC[F" -- move up one line
      , "\ESC[K" -- clear to end of line
      , "\ESC[", show indent, "C" -- move indent characters right
      , leftEdge, replicate numFull '█', rightEdge
      ]

    -- smooth out timing from threadDelay so the entire animation
    -- takes no longer than desired
    --
    -- probably undesirable in word presentation
    currTime <- timeSpecToMicroseconds <$> getTime Monotonic
    let delay = round $ startTime + (fromIntegral frameNum + 1)/framesPerMicrosecond - currTime
    threadDelay delay

  putStrLn "\ESC[?25h" -- show cursor
  endTime <- timeSpecToMicroseconds <$> getTime Monotonic
  print $ (round $ endTime - startTime :: Integer)
