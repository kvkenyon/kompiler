{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.Text as T
import Parser (readExpr)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )

process :: String -> IO ()
process line = do
  let res = readExpr $ T.pack line
  case res of
    Left err -> print err
    Right ex -> print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
