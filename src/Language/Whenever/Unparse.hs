module Language.Whenever.Unparse where

import Language.Whenever.Base
import Data.List (intercalate)

expr :: Expr -> String
expr e = case e of
  Val v -> case v of
    Str s -> show s
    Int i -> show i
    Bool b -> if b then "true" else "false"
  Binop op x y -> concat ["(", expr x, " ", operator, " ", expr y, ")"] where
    operator = case op of
      Plus -> "+"
      Sub -> "-"
      Mult -> "*"
      Div -> "/"
      Or -> "||"
      And -> "&&"
      Less -> "<"
      Greater -> ">"
      LessEqual -> "<="
      GreaterEqual -> ">="
      Equal -> "=="
      NotEqual -> "!="
  Not x -> "!" ++ expr x
  Read -> "read()"
  Print x -> concat ["print(", expr x, ")"]
  N x -> concat ["N(", expr x, ")"]
  U x -> concat ["U(", expr x, ")"]
  If c t f -> concat ["(", expr c, " ? ", expr t, " : ", expr f, ")"]

stmt :: Stmt -> String
stmt s = case s of
  Defer x s' -> concat ["defer (", expr x, ") ", stmt s']
  Again x s' -> concat ["again (", expr x, ") ", stmt s']
  Commands cs -> intercalate ", " [ expr x ++ "#" ++ expr y | (x, y) <- cs ] 

line :: LineNumber -> Stmt -> String
line n s = concat [show n, " ", stmt s, ";"]

program :: Program -> String
program = unlines . map (uncurry line)
