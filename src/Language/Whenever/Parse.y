{
module Language.Whenever.Parse (parse) where

import qualified Data.Map as Map

import Language.Whenever.Base
import qualified Language.Whenever.Scan as S
}

%name parse
%tokentype { S.Token }
%error { parseError }

%token
  int { S.Int $$ }
  str { S.Str $$ }
  bool { S.Bool $$ }
  defer { S.Defer }
  again { S.Again }
  n { S.N }
  u { S.U }
  print { S.Print }
  read { S.Read }
  '(' { S.LParen }
  ')' { S.RParen }
  ';' { S.Semi }
  ',' { S.Comma }
  '#' { S.Hash }
  '+' { S.Plus }
  '-' { S.Minus }
  '*' { S.Star }
  '/' { S.Slash }
  '%' { S.Percent }
  '||' { S.Or }
  '&&' { S.And }
  '==' { S.Equal }
  '!=' { S.NotEqual }
  '<' { S.Less }
  '>' { S.Greater }
  '<=' { S.LessEqual }
  '>=' { S.GreaterEqual }
  '!' { S.Not }
  '?' { S.Question }
  ':' { S.Colon }

%%

File : { [] }
     | Line ';' File { $1 : $3 }

Line : int Stmt { ($1, $2) }

Stmt : defer '(' Expr ')' Stmt { Defer (getBool $3) $5 }
     | again '(' Expr ')' Stmt { Again (getBool $3) $5 }
     | Commands { Commands $1 }

Commands : Command { [$1] }
         | Command ',' Commands { $1 : $3 }

Command : Expr { (getInt $1, Val 1) }
        | Expr '#' Expr { (getInt $1, getInt $3) }

Expr : Expr0 { $1 }

{-
TODO. An if expression can have multiple types, how do we handle that?
Worst case you could have an expression like:
  print(3 + (20 ? 'a' : 4))
where the ternary result changes whether + means add or append.

Expr : Expr0 '?' Expr ':' Expr { If $1 $3 $5 }
     | Expr0 { $1 }
-}

Expr0 : Expr0 '||' Expr1 { Bool $ Or (getBool $1) (getBool $3) }
      | Expr1 { $1 }

Expr1 : Expr1 '&&' Expr2 { Bool $ And (getBool $1) (getBool $3) }
      | Expr2 { $1 }

Expr2 : Expr2 '==' Expr3 { Bool $ EqualStr (getStr $1) (getStr $3) }
      | Expr2 '!=' Expr3 { Bool $ Not $ EqualStr (getStr $1) (getStr $3) }
      | Expr3 { $1 }

Expr3 : Expr3 '<' Expr4 { Bool $ Less (getInt $1) (getInt $3) }
      | Expr3 '>' Expr4 { Bool $ Greater (getInt $1) (getInt $3) }
      | Expr3 '<=' Expr4 { Bool $ LessEqual (getInt $1) (getInt $3) }
      | Expr3 '>=' Expr4 { Bool $ GreaterEqual (getInt $1) (getInt $3) }
      | Expr4 { $1 }

Expr4 : Expr4 '+' Expr5 { plus $1 $3 }
      | Expr4 '-' Expr5 { Int $ Sub (getInt $1) (getInt $3) }
      | Expr5 { $1 }

Expr5 : Expr5 '*' Atom { Int $ Mult (getInt $1) (getInt $3) }
      | Expr5 '/' Atom { Int $ Div (getInt $1) (getInt $3) }
      | Expr5 '%' Atom { Int $ Rem (getInt $1) (getInt $3) }
      | Atom { $1 }

Atom : int { Int $ Val $1 }
     | str { Str $ Val $1 }
     | bool { Bool $ Val $1 }
     | n '(' Expr ')' { Int $ N $ getInt $3 }
     | u '(' Expr ')' { Str $ U $ getInt $3 }
     | print '(' Expr ')' { Int $ Print $ getStr $3 }
     | read '(' ')' { Int Read }
     | '!' Atom { Bool $ Not $ getBool $2 }
     | '(' Expr ')' { $2 }

{

data Generic
  = Str (Expr String)
  | Int (Expr Integer)
  | Bool (Expr Bool)
  deriving (Eq, Show)

plus :: Generic -> Generic -> Generic
plus (Str x) y       = Str $ Append x (getStr y)
plus x       (Str y) = Str $ Append (getStr x) y
plus x       y       = Int $ Add (getInt x) (getInt y)

getStr :: Generic -> Expr String
getStr (Str s) = s
getStr (Int i) = IntToStr i
getStr (Bool b) = BoolToStr b

getInt :: Generic -> Expr Integer
getInt (Str s) = StrToInt s
getInt (Int i) = i
getInt (Bool b) = BoolToInt b

getBool :: Generic -> Expr Bool
getBool (Str s) = StrToBool s
getBool (Int i) = IntToBool i
getBool (Bool b) = b

parseError :: [S.Token] -> a
parseError _ = error "Parse error"

}
