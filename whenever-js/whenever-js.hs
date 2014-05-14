{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings #-}
module Main where

import GHCJS.Foreign
import GHCJS.Types
import Language.Whenever
import Control.Applicative ((<$>), (<*))

foreign import javascript unsafe
  "document.getElementById('whenever_program').value"
  programContents :: IO JSString

foreign import javascript unsafe
  "document.getElementById('whenever_output').value += $1;"
  outputStr :: JSString -> IO ()

foreign import javascript unsafe
  "Math.floor(Math.random() * $1)"
  randomInt :: Int -> IO Int

foreign import javascript interruptible
  "lookAhead($c);"
  lookAhead :: IO Char

foreign import javascript interruptible
  "getChar($c);"
  jsGetChar :: IO Char

main :: IO ()
main = do
  prog <- fromString . fromJSString <$> programContents
  let wio = WheneverIO
        { wPrint = outputStr . toJSString
        , wLook = lookAhead
        , wGet = jsGetChar
        , wRandom = randomInt
        }
  runProgram wio prog
