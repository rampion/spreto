{-# LANGUAGE RecordWildCards #-}
module Main where
-- Alternate intro demo 
--  instead of ticking in from both sides at different speeds,
--  tick in from just the longer side until both sides are the same length
--  then tick in from both at the same speed
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

      numFrames = lcm numLeftEdges numRightEdges * max prefixWidth suffixWidth

      framesPerMicrosecond = fromIntegral numFrames  / durationInMicroseconds

      reticule c = replicate prefixWidth '─' ++ c : replicate suffixWidth '─'

  putStr "\ESC[?25l" -- hide cursor
  putStrLn $ reticule '┬'
  putStrLn ""
  putStr $ reticule '┴'
  putStrLn "\ESC[F" -- move up one line

  startTime <- timeSpecToMicroseconds <$> getTime Monotonic

  forM_ [0..numFrames] $ \frameNum -> do
    let framesRemaining = numFrames - frameNum
        (full, partial) = (max prefixWidth suffixWidth * framesRemaining) `quotRem` numFrames
        (leftFull, leftEdge) 
          | prefixWidth <= full = (prefixWidth, "")
          | otherwise           = (full, leftEdges ! quot (numLeftEdges * partial) numFrames)
        (rightFull, rightEdge) 
          | suffixWidth <= full = (suffixWidth, "")
          | otherwise           = (full, rightEdges ! quot (numRightEdges * partial) numFrames)
        numFull = leftFull + 1 + rightFull
        indent = prefixWidth - leftFull - length leftEdge

    putStr $ concat
      [ "\ESC[F" -- move up one line
      , "\ESC[K" -- clear to end of line
      ]
    when (indent > 0) . putStr $ concat
      [ "\ESC[", show indent, "C" ] -- move indent characters right; \ESC[0C moves 1, not 0
    putStrLn $ concat
      [ leftEdge, replicate numFull '█', rightEdge ]

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
