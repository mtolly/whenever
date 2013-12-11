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

Stmt : defer '(' Expr ')' Stmt { Defer (optimize $ AnyToBool $3) $5 }
     | again '(' Expr ')' Stmt { Again (optimize $ AnyToBool $3) $5 }
     | Commands { Commands $1 }

Commands : Command { [$1] }
         | Command ',' Commands { $1 : $3 }

Command : Expr { (optimize $ AnyToInt $1, Val 1) }
        | Expr '#' Expr { (optimize $ AnyToInt $1, optimize $ AnyToInt $3) }

Expr : Expr0 '?' Expr ':' Expr { If (AnyToBool $1) $3 $5 }
     | Expr0 { $1 }

Expr0 : Expr0 '||' Expr1 { BoolToAny $ Or (AnyToBool $1) (AnyToBool $3) }
      | Expr1 { $1 }

Expr1 : Expr1 '&&' Expr2 { BoolToAny $ And (AnyToBool $1) (AnyToBool $3) }
      | Expr2 { $1 }

Expr2 : Expr2 '==' Expr3 { BoolToAny $ Equal $1 $3 }
      | Expr2 '!=' Expr3 { BoolToAny $ Not $ Equal $1 $3 }
      | Expr3 { $1 }

Expr3 : Expr3 '<' Expr4 { BoolToAny $ Less (AnyToInt $1) (AnyToInt $3) }
      | Expr3 '>' Expr4 { BoolToAny $ Greater (AnyToInt $1) (AnyToInt $3) }
      | Expr3 '<=' Expr4 { BoolToAny $ LessEqual (AnyToInt $1) (AnyToInt $3) }
      | Expr3 '>=' Expr4 { BoolToAny $ GreaterEqual (AnyToInt $1) (AnyToInt $3) }
      | Expr4 { $1 }

Expr4 : Expr4 '+' Expr5 { Plus $1 $3 }
      | Expr4 '-' Expr5 { IntToAny $ Sub (AnyToInt $1) (AnyToInt $3) }
      | Expr5 { $1 }

Expr5 : Expr5 '*' Atom { IntToAny $ Mult (AnyToInt $1) (AnyToInt $3) }
      | Expr5 '/' Atom { IntToAny $ Div (AnyToInt $1) (AnyToInt $3) }
      | Expr5 '%' Atom { IntToAny $ Rem (AnyToInt $1) (AnyToInt $3) }
      | Atom { $1 }

Atom : int { IntToAny $ Val $1 }
     | str { StrToAny $ Val $1 }
     | bool { BoolToAny $ Val $1 }
     | n '(' Expr ')' { IntToAny $ N $ AnyToInt $3 }
     | u '(' Expr ')' { StrToAny $ U $ AnyToInt $3 }
     | print '(' Expr ')' { IntToAny $ Print $ AnyToStr $3 }
     | read '(' ')' { IntToAny Read }
     | '!' Atom { BoolToAny $ Not $ AnyToBool $2 }
     | '(' Expr ')' { $2 }

{

parseError :: [S.Token] -> a
parseError _ = error "Parse error"

}
