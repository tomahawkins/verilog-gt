module Main (main) where

import Data.List
import System.Environment

import Language.Verilog

import Inline

main :: IO ()
main = do
  args <- getArgs
  let (defines, files) = parseArgs args
  modules <- mapM (\ file -> readFile file >>= return . parseFile defines file) files >>= return . concat
  print $ inline modules

parseArgs :: [String] -> ([(String, String)], [FilePath])
parseArgs args = (concat a, concat b)
  where
  (a, b) = unzip $ map parseArg args
  parseArg :: String -> ([(String, String)], [FilePath])
  parseArg a
    | isPrefixOf "-D" a && elem '=' a = ([(takeWhile (/= '=') $ drop 2 a, tail $ dropWhile (/= '=') a)], [])
    | isPrefixOf "-D" a               = ([(drop 2 a, "")], [])
    | otherwise                       = ([], [a])
    
