module Main where

import Language.Whenever
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [f] -> runFile f
  _ -> hPutStrLn stderr "Usage: whenever <file-in>"
