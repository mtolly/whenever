module Language.Whenever
( module Language.Whenever.Base
, fromString
, fromFile
, runFile
, module Language.Whenever.Unparse
) where

import Language.Whenever.Base
import Language.Whenever.Scan (scan)
import Language.Whenever.Parse (parse)
import Language.Whenever.Unparse

fromString :: String -> Program
fromString = parse . scan

fromFile :: FilePath -> IO Program
fromFile = fmap fromString . readFile

runFile :: FilePath -> IO ()
runFile f = fromFile f >>= runProgram
