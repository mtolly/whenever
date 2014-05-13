{
{-# OPTIONS_GHC -w #-}
module Language.Whenever.Scan (scan, Token(..)) where

import qualified Data.Text as T
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

$white+ ;
\/\/ ([^\n]*) ;

\-? ($digit)+ { Int . read }
\" ([^ \" \\] | (\\ .))* \" { Str . read }
defer { const Defer }
again { const Again }
N { const N }
U { const U }
print { const Print }
read { const Read }
\( { const LParen }
\) { const RParen }
\; { const Semi }
\, { const Comma }
\# { const Hash }
\+ { const Plus }
\- { const Minus }
\* { const Star }
\/ { const Slash }
\% { const Percent }
\|\| { const Or }
\&\& { const And }
\=\= { const Equal }
\!\= { const NotEqual }
\< { const Less }
\> { const Greater }
\<\= { const LessEqual }
\>\= { const GreaterEqual }
\! { const Not }
true { const (Bool True) }
false { const (Bool False) }
\? { const Question }
\: { const Colon }

{

data Token
  = Int Integer
  | Str T.Text
  | Bool Bool
  | Defer
  | Again
  | N
  | U
  | Print
  | Read
  | LParen
  | RParen
  | Semi
  | Comma
  | Hash
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Or
  | And
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | Not
  | Question
  | Colon
  deriving (Eq, Ord, Show, Read)

scan :: String -> [Token]
scan = alexScanTokens

}
