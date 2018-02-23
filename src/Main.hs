{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Brick
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Graphics.Vty as Vty

import Cursor (cursor)
import Document (parseDocument)
import Options (Options(..), options, execParser)
import UI 

orElseM :: Monad m => Maybe a -> m a -> m a
orElseM (Just a) _ = return a
orElseM _ ma       = ma

main :: IO ()
main = do
  Options{..} <- execParser options
  document <- parseDocument <$> TextIO.readFile path
  here <- cursor document initialPos `orElseM` do
    TextIO.hPutStrLn stderr $ Text.unwords 
      [ "ERROR: illegal start position"
      , Text.pack $ show initialPos
      , "in document"
      , Text.pack $ path
      ]
    exitFailure

  _st <- runUI here Config
    { uiIntroDuration     = if skipIntro then Nothing else Just 3e6
    , uiUnpauseDuration   = if skipIntro then Nothing else Just 1e6
    , uiAttributes        = Brick.attrMap Vty.defAttr 
                              [ (wordFocus, Brick.fg Vty.yellow)
                              , (contextPrefix, Brick.fg Vty.cyan)
                              , (contextSuffix, Brick.fg Vty.cyan)
                              ]
    , uiWPM               = initialWpm
    , uiReadingDirection  = Forwards
    , uiProgressDisplay   = Percentage
    }
  return ()
