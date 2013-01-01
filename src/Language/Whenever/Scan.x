{
{-# OPTIONS_GHC -w #-}
module Language.Whenever.Scan (scan, Token(..)) where
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
\|\| { const Or }
\&\& { const And }
\=\= { const Equal }
\!\= { const NotEqual }
\< { const Less }
\> { const Greater }
\<\= { const LessEqual }
\>\= { const GreaterEqual }
\! { const Not }
true { const WTrue }
false { const WFalse }

{

data Token
  = Int Integer
  | Str String
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
  | Or
  | And
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | Not
  | WTrue
  | WFalse
  deriving (Eq, Ord, Show, Read)

scan :: String -> [Token]
scan = alexScanTokens

}
