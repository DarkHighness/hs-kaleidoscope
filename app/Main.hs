{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified LLVM.AST as AST
import Lang.Codegen (emptyModule)
import Lang.Emit (codegen)
import Lang.Parser (parseTopLevel)
import System.Console.Haskeline
import System.Environment (getArgs)

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> Text -> IO (Maybe AST.Module)
process modo source = do
  let res = parseTopLevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: FilePath -> IO (Maybe AST.Module)
processFile fname = do
  content <- T.readFile fname
  process initModule content

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          modn <- liftIO $ process mod (T.pack input)
          case modn of
            Just modn -> loop modn
            Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> void (processFile fname)