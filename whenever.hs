module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Language.Whenever

main :: IO ()
main = getArgs >>= \argv -> case argv of
  ["-?"] -> printUsage
  [f]    -> runFile f
  []     -> getContents >>= runProgram . fromString
  _      -> printUsage >> exitFailure

printUsage :: IO ()
printUsage = mapM_ (hPutStrLn stderr)
  [ "whenever: language by David Morgan-Mar <dmm@dangermouse.net>"
  , "          interpreter in Haskell by Michael Tolly <miketolly@gmail.com>"
  , "Usage: whenever file.wr < stdin"
  , "       whenever < file.wr"
  ]
