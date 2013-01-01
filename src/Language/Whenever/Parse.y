{
module Language.Whenever.Parse (parse) where

import qualified Language.Whenever.Scan as S
import Language.Whenever.Base
import qualified Data.Map as Map
}

%name parse
%tokentype { S.Token }
%error { parseError }

%token
  int { S.Int $$ }
  str { S.Str $$ }
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
  '||' { S.Or }
  '&&' { S.And }
  '==' { S.Equal }
  '!=' { S.NotEqual }
  '<' { S.Less }
  '>' { S.Greater }
  '<=' { S.LessEqual }
  '>=' { S.GreaterEqual }
  '!' { S.Not }
  true { S.WTrue }
  false { S.WFalse }

%%

File : { [] }
     | Line ';' File { $1 : $3 }

Line : int Stmt { ($1, $2) }

Stmt : defer '(' Expr ')' Stmt { Defer $3 $5 }
     | again '(' Expr ')' Stmt { Again $3 $5 }
     | Commands { Commands $1 }

Commands : Command { [$1] }
         | Command ',' Commands { $1 : $3 }

Command : Expr { ($1, Val $ Int 1) }
        | Expr '#' Expr { ($1, $3) }

Expr : Expr '||' Expr1 { Binop Or $1 $3 }
     | Expr1 { $1 }

Expr1 : Expr1 '&&' Expr2 { Binop And $1 $3 }
      | Expr2 { $1 }

Expr2 : Expr2 '==' Expr3 { Binop Equal $1 $3 }
      | Expr2 '!=' Expr3 { Binop NotEqual $1 $3 }
      | Expr3{ $1 }

Expr3 : Expr3 '<' Expr4 { Binop Less $1 $3 }
      | Expr3 '>' Expr4 { Binop Greater $1 $3 }
      | Expr3 '<=' Expr4 { Binop LessEqual $1 $3 }
      | Expr3 '>=' Expr4 { Binop GreaterEqual $1 $3 }
      | Expr4 { $1 }

Expr4 : Expr4 '+' Expr5 { Binop Plus $1 $3 }
      | Expr4 '-' Expr5 { Binop Sub $1 $3 }
      | Expr5 { $1 }

Expr5 : Expr5 '*' Atom { Binop Mult $1 $3 }
      | Expr5 '/' Atom { Binop Div $1 $3 }
      | Atom { $1 }

Atom : int { Val (Int $1) }
     | str { Val (Str $1) }
     | n '(' Expr ')' { N $3 }
     | u '(' Expr ')' { U $3 }
     | print '(' Expr ')' { Print $3 }
     | read '(' ')' { Read }
     | '!' Atom { Not $2 }
     | '(' Expr ')' { $2 }
     | true { Val (Bool True) }
     | false { Val (Bool False) }

{

parseError :: [S.Token] -> a
parseError _ = error "Parse error"

}
