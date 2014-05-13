{-# LANGUAGE GADTs, OverloadedStrings #-}
module Language.Whenever.Unparse
( expr
, stmt
, line
, program
, Value
, showProgram
) where

import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

import Language.Whenever.Base

class Value a where
  toBuilder :: a -> B.Builder

instance Value T.Text where
  toBuilder = B.fromString . show

instance Value Integer where
  toBuilder = B.fromString . show

instance Value Bool where
  toBuilder b = B.fromText $ if b then "true" else "false"

instance Value Any where
  toBuilder (Str s) = toBuilder s
  toBuilder (Int i) = toBuilder i
  toBuilder (Bool b) = toBuilder b

-- | Generates the code for an expression. If the `Bool` is `True`, a binary or
-- ternary operator will have parentheses around it. Subexpressions always have
-- enough parentheses so that operator precedence is unambiguous.
expr :: (Value a) => Bool -> Expr a -> B.Builder
expr atom e = case e of
  Val v -> toBuilder v
  Append x y -> op "+" x y
  Add x y -> op "+" x y
  Plus x y -> op "+" x y
  Sub x y -> op "-" x y
  Mult x y -> op "*" x y
  Div x y -> op "/" x y
  Rem x y -> op "%" x y
  Or x y -> op "||" x y
  And x y -> op "&&" x y
  Compare o x y -> op (case o of LT -> "<"; EQ -> "=="; GT -> "<") x y
  Equal x y -> op "==" x y
  EqualStr x y -> op "==" x y
  EqualBool x y -> op "==" x y
  Not (Equal x y) -> op "!=" x y
  Not (EqualStr x y) -> op "!=" x y
  Not (EqualBool x y) -> op "!=" x y
  Not (Compare o x y) -> op (case o of LT -> ">="; EQ -> "!="; GT -> "<=") x y
  Not x -> "!" <> expr True x
  Read -> "read()"
  Print xs -> case xs of
    [x] -> mconcat ["print(", expr False x, ")"]
    []  -> "print(\"\")"
    _   -> expr atom $ Print [foldr Append (Val "") xs]
  N x -> mconcat ["N(", expr False x, ")"]
  U x -> mconcat ["U(", expr False x, ")"]
  If c t f -> atomize $ mconcat
    [expr True c, " ? ", expr True t, " : ", expr True f]
  IntToStr x -> atomize $ expr True x <> " + \"\""
  StrToInt x -> atomize $ expr True x <> " - 0"
  BoolToInt x -> atomize $ expr True x <> " ? 1 : 0"
  IntToBool x -> atomize $ expr True x <> " && true"
  BoolToStr x -> atomize $ expr True x <> " ? \"true\" : \"false\""
  StrToBool x -> atomize $ expr True x <> " ? 1 : 0"
  AnyToStr x -> atomize $ expr True x <> " + \"\""
  AnyToInt x -> atomize $ expr True x <> " - 0"
  AnyToBool x -> atomize $ expr True x <> " && true"
  StrToAny x -> expr atom x
  IntToAny x -> expr atom x
  BoolToAny x -> expr atom x
  where atomize s = if atom then "(" <> s <> ")" else s
        op :: (Value a, Value b) => B.Builder -> Expr a -> Expr b -> B.Builder
        op o x y = atomize $ mconcat [expr True x, " ", o, " ", expr True y]

-- | Generates the code for a statement.
stmt :: Stmt -> B.Builder
stmt s = case s of
  Defer x s' -> mconcat ["defer (", expr False x, ") ", stmt s']
  Again x s' -> mconcat ["again (", expr False x, ") ", stmt s']
  Commands cs -> mconcat $ intersperse ", "
    [ case y of
        Val 1 -> expr True x
        _     -> expr True x <> "#" <> expr True y
    | (x, y) <- cs ] 

-- | Generates the code for a line with a number and a statement.
line :: LineNumber -> Stmt -> B.Builder
line n s = mconcat [toBuilder n, " ", stmt s, ";"]

-- | Generates the code for a complete program.
program :: Program -> B.Builder
program = mconcat . intersperse "\n" . map (uncurry line)

showProgram :: Program -> TL.Text
showProgram = B.toLazyText . program
