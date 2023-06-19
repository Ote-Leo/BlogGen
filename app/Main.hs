{-# LANGUAGE LambdaCase #-}

module Main where

import qualified HsBlog
import OptParse
import System.Directory
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output -> HsBlog.convertDirectory input output
    ConvertSingle input output -> do
      (title, inputHandle) <- case input of
        Stdin -> return ("", stdin)
        InputFile file -> (,) file <$> openFile file ReadMode

      outputHandle <- case output of
        Stdout -> return stdout
        OutputFile file -> do
          exists <- doesFileExist file
          shouldOpenFile <- if exists then confirm else return True
          if shouldOpenFile then openFile file WriteMode else exitFailure

      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

confirm :: IO Bool
confirm =
  putStrLn "Are your sure? (y/n)"
    >> getLine >>= \case
      "y" -> return True
      "n" -> return False
      _ -> putStrLn "Invalid response. Use either \"y\" or \"n\"" >> confirm
