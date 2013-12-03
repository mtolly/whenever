module Language.Whenever.Unparse
( expr
, stmt
, line
, program
) where

import Data.List (intercalate)

import Language.Whenever.Base

-- | Generates the code for an expression. If the `Bool` is `True`, a binary or
-- ternary operator will have parentheses around it. Subexpressions always have
-- enough parentheses so that operator precedence is unambiguous.
expr :: Bool -> Expr -> String
expr atom e = case e of
  Val v -> case v of
    Str s -> show s
    Int i -> show i
    Bool b -> if b then "true" else "false"
  Binop op x y -> atomize $ unwords [expr True x, operator, expr True y] where
    operator = case op of
      Plus -> "+"
      Sub -> "-"
      Mult -> "*"
      Div -> "/"
      Rem -> "%"
      Or -> "||"
      And -> "&&"
      Less -> "<"
      Greater -> ">"
      LessEqual -> "<="
      GreaterEqual -> ">="
      Equal -> "=="
      NotEqual -> "!="
  Not x -> '!' : expr True x
  Read -> "read()"
  Print x -> concat ["print(", expr False x, ")"]
  N x -> concat ["N(", expr False x, ")"]
  U x -> concat ["U(", expr False x, ")"]
  If c t f -> atomize $ unwords
    [expr True c, "?", expr True t, ":", expr True f]
  where atomize s = if atom then "(" ++ s ++ ")" else s

-- | Generates the code for a statement.
stmt :: Stmt -> String
stmt s = case s of
  Defer x s' -> concat ["defer (", expr False x, ") ", stmt s']
  Again x s' -> concat ["again (", expr False x, ") ", stmt s']
  Commands cs -> intercalate ", "
    [ case y of
        Val (Int 1) -> expr True x
        _           -> expr True x ++ "#" ++ expr True y
    | (x, y) <- cs ] 

-- | Generates the code for a line with a number and a statement.
line :: LineNumber -> Stmt -> String
line n s = concat [show n, " ", stmt s, ";"]

-- | Generates the code for a complete program.
program :: Program -> String
program = unlines . map (uncurry line)
