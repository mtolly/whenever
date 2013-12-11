{-# LANGUAGE GADTs #-}
module Language.Whenever.Unparse
( expr
, stmt
, line
, program
) where

import Data.Char (toLower)
import Data.List (intercalate)

import Language.Whenever.Base

-- | Generates the code for an expression. If the `Bool` is `True`, a binary or
-- ternary operator will have parentheses around it. Subexpressions always have
-- enough parentheses so that operator precedence is unambiguous.
expr :: (Show a) => Bool -> Expr a -> String
expr atom e = case e of
  Val v -> map toLower $ show v
  Append x y -> op "+" x y
  Add x y -> op "+" x y
  Plus x y -> op "+" x y
  Sub x y -> op "-" x y
  Mult x y -> op "*" x y
  Div x y -> op "/" x y
  Rem x y -> op "%" x y
  Or x y -> op "||" x y
  And x y -> op "&&" x y
  Less x y -> op "<" x y
  Greater x y -> op ">" x y
  LessEqual x y -> op "<=" x y
  GreaterEqual x y -> op ">=" x y
  Equal x y -> op "==" x y
  EqualStr x y -> op "==" x y
  EqualInt x y -> op "==" x y
  EqualBool x y -> op "==" x y
  Not (Equal x y) -> op "!=" x y
  Not (EqualStr x y) -> op "!=" x y
  Not (EqualInt x y) -> op "!=" x y
  Not (EqualBool x y) -> op "!=" x y
  Not x -> '!' : expr True x
  Read -> "read()"
  Print xs -> case xs of
    [x] -> concat ["print(", expr False x, ")"]
    []  -> "print(\"\")"
    _   -> expr atom $ Print [foldr Append (Val "") xs]
  N x -> concat ["N(", expr False x, ")"]
  U x -> concat ["U(", expr False x, ")"]
  If c t f -> atomize $ unwords
    [expr True c, "?", expr True t, ":", expr True f]
  IntToStr x -> atomize $ expr True x ++ " + \"\""
  StrToInt x -> atomize $ expr True x ++ " - 0"
  BoolToInt x -> atomize $ expr True x ++ " ? 1 : 0"
  IntToBool x -> atomize $ expr True x ++ " && true"
  BoolToStr x -> atomize $ expr True x ++ " ? \"true\" : \"false\""
  StrToBool x -> atomize $ expr True x ++ " ? 1 : 0"
  AnyToStr x -> atomize $ expr True x ++ " + \"\""
  AnyToInt x -> atomize $ expr True x ++ " - 0"
  AnyToBool x -> atomize $ expr True x ++ " && true"
  StrToAny x -> expr atom x
  IntToAny x -> expr atom x
  BoolToAny x -> expr atom x
  where atomize s = if atom then "(" ++ s ++ ")" else s
        op :: (Show a, Show b) => String -> Expr a -> Expr b -> String
        op o x y = atomize $ unwords [expr True x, o, expr True y]

-- | Generates the code for a statement.
stmt :: Stmt -> String
stmt s = case s of
  Defer x s' -> concat ["defer (", expr False x, ") ", stmt s']
  Again x s' -> concat ["again (", expr False x, ") ", stmt s']
  Commands cs -> intercalate ", "
    [ case y of
        Val 1 -> expr True x
        _     -> expr True x ++ "#" ++ expr True y
    | (x, y) <- cs ] 

-- | Generates the code for a line with a number and a statement.
line :: LineNumber -> Stmt -> String
line n s = concat [show n, " ", stmt s, ";"]

-- | Generates the code for a complete program.
program :: Program -> String
program = unlines . map (uncurry line)
