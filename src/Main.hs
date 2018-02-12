{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Data.Semigroup ((<>))
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Brick
import qualified Brick.BChan as BChan
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Graphics.Vty as Vty

import Cursor (cursor)
import Document (parseDocument)
import Position (Position(..))
import Options
import UI

orElseM :: Monad m => Maybe a -> m a -> m a
orElseM (Just a) _ = return a
orElseM _ ma       = ma

main :: IO ()
main = do
  Options{..} <- execParser options
  events <- BChan.newBChan 10 -- Q: why 10? A: stolen from the example
  document <- parseDocument <$> TextIO.readFile path
  here <- cursor document initialPos `orElseM` do
    TextIO.hPutStrLn stderr $ Text.unwords 
      [ "ERROR: illegal start position"
      , Text.pack $ show initialPos
      , "in document"
      ,  path
      ]
    exitFailure

  -- prime the pump
  BChan.writeBChan events Advance

  -- XXX: takes over the entire display, which is non-optimal
  --      see https://github.com/jtdaugherty/vty/issues/143
  _st <- (Brick.customMain (Vty.mkVty Vty.defaultConfig) (Just events) . makeApp)
          Env
            { introDuration   = if skipIntro then Nothing else Just 5e6
            , unpauseDuration = if skipIntro then Nothing else Just 5e6
            , reticuleWidth   = 80 -- Q: why 80? A: 80 columns is a "standard" code width
            , events          = events
            , attributes      = attrMap Vty.defAttr [(wordFocus, Brick.fg Vty.red)]
            }
          State
            { wpm       = initialWpm
            , direction = Forwards
            , mode      = Starting
            , here      = here
            }

  -- clear the bottom line if exit after intro
  putStrLn ""
  putStrLn ""
  -- TODO: print final position?

