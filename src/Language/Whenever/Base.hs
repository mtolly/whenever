{-# LANGUAGE StandaloneDeriving, GADTs #-}
module Language.Whenever.Base
( LineNumber
, Count
, Expr(..)
, Stmt(..)
, Program
, Context
, Whenever
, eval
, count
, setCount
, addLine
, getStmt
, runLine
, runStmt
, select
, run
, makeContext
, runProgram
, Generic(..)
, getStr, getInt, getBool
, plus
) where

import Control.Applicative ((<$>), liftA2)
import Control.Monad (forM_, unless)
import Data.Char (isDigit)
import System.IO (hLookAhead, stdin)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import qualified Data.Map as Map
import System.Random (getStdRandom, randomR)

type LineNumber = Integer
type Count = Integer

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

data Expr a where
  Val          :: a -> Expr a
  Append       :: Expr String -> Expr String -> Expr String
  Add          :: Expr Integer -> Expr Integer -> Expr Integer
  Sub          :: Expr Integer -> Expr Integer -> Expr Integer
  Mult         :: Expr Integer -> Expr Integer -> Expr Integer
  Div          :: Expr Integer -> Expr Integer -> Expr Integer
  Rem          :: Expr Integer -> Expr Integer -> Expr Integer
  Or           :: Expr Bool -> Expr Bool -> Expr Bool
  And          :: Expr Bool -> Expr Bool -> Expr Bool
  Less         :: Expr Integer -> Expr Integer -> Expr Bool
  Greater      :: Expr Integer -> Expr Integer -> Expr Bool
  LessEqual    :: Expr Integer -> Expr Integer -> Expr Bool
  GreaterEqual :: Expr Integer -> Expr Integer -> Expr Bool
  EqualStr     :: Expr String -> Expr String -> Expr Bool
  EqualInt     :: Expr Integer -> Expr Integer -> Expr Bool
  EqualBool    :: Expr Bool -> Expr Bool -> Expr Bool
  Not          :: Expr Bool -> Expr Bool
  Read         :: Expr Integer
  Print        :: Expr String -> Expr Integer
  N            :: Expr Integer -> Expr Integer
  U            :: Expr Integer -> Expr String
  If           :: Expr Bool -> Expr a -> Expr a -> Expr a
  IntToStr     :: Expr Integer -> Expr String
  StrToInt     :: Expr String -> Expr Integer
  BoolToInt    :: Expr Bool -> Expr Integer
  IntToBool    :: Expr Integer -> Expr Bool
  BoolToStr    :: Expr Bool -> Expr String
  StrToBool    :: Expr String -> Expr Bool

deriving instance (Eq a) => Eq (Expr a)
deriving instance (Show a) => Show (Expr a)

data Stmt
  = Defer (Expr Bool) Stmt
  | Again (Expr Bool) Stmt
  | Commands [(Expr Integer, Expr Integer)]
  deriving (Eq, Show)

type Program = [(LineNumber, Stmt)]
type Context = Map.Map LineNumber (Count, Stmt)
type Whenever = StateT Context IO

-- | Evaluates an expression to return a value. This may have side-effects
-- (executing a 'Language.Whenever.Base.Read' or 'Print' expression) but won't
-- modify any line's count.
eval :: Expr a -> Whenever a
eval e = case e of
  Val          v   -> return v
  Append       x y -> liftA2 (++) (eval x) (eval y)
  Add          x y -> liftA2 (+)  (eval x) (eval y)
  Sub          x y -> liftA2 (-)  (eval x) (eval y)
  Mult         x y -> liftA2 (*)  (eval x) (eval y)
  Div          x y -> liftA2 quot (eval x) (eval y)
  Rem          x y -> liftA2 rem  (eval x) (eval y)
  Or           x y -> liftA2 (||) (eval x) (eval y)
  And          x y -> liftA2 (&&) (eval x) (eval y)
  Less         x y -> liftA2 (<)  (eval x) (eval y)
  Greater      x y -> liftA2 (>)  (eval x) (eval y)
  LessEqual    x y -> liftA2 (<=) (eval x) (eval y)
  GreaterEqual x y -> liftA2 (>=) (eval x) (eval y)
  EqualStr     x y -> liftA2 (==) (eval x) (eval y)
  EqualInt     x y -> liftA2 (==) (eval x) (eval y)
  EqualBool    x y -> liftA2 (==) (eval x) (eval y)
  Not          x   -> not <$> eval x
  Read             -> readInput
  Print        x   -> eval x >>= lift . putStrLn >> return 0
  N            x   -> eval x >>= count
  U            x   -> (: "") . toEnum . fromIntegral <$> eval x
  If         c t f -> eval c >>= \b -> eval $ if b then t else f
  IntToStr     x   -> show <$> eval x
  StrToInt     x   -> strToInt <$> eval x
  BoolToInt    x   -> fromIntegral . fromEnum <$> eval x
  IntToBool    x   -> eval x >>= intToBool
  BoolToStr    x   -> (\b -> if b then "true" else "false") <$> eval x
  StrToBool    x   -> eval x >>= intToBool . strToInt

strToInt :: String -> Integer
strToInt s = case reads s of
  [(i, _)] -> i
  _        -> 0

intToBool :: Integer -> Whenever Bool
intToBool i = (/= 0) <$> count i

-- | From standard input, reads either a natural number (sequence of digits),
-- or a single char (returning its codepoint) if it isn't a digit.
readInput :: Whenever Integer
readInput = lift $ hLookAhead stdin >>= \c -> if isDigit c
  then let
    readWhile p = hLookAhead stdin >>= \x -> if p x
      then liftA2 (:) getChar $ readWhile p
      else return ""
    in read <$> readWhile isDigit
  else fromIntegral . fromEnum <$> getChar

-- | Returns the number of copies of a line.
count :: LineNumber -> Whenever Count
count n = maybe 0 fst <$> gets (Map.lookup n)

-- | Sets the number of copies of a line. A negative count sets to zero.
-- Setting to zero doesn't remove the line from memory; it might be readded
-- later.
setCount :: LineNumber -> Count -> Whenever ()
setCount n c = modify $ \m -> case Map.lookup n m of
  Just (_, x) -> Map.insert n (max 0 c, x) m
  Nothing     -> error $ "setCount: unknown line number " ++ show n

-- | Adds to (positive) or subtracts from (negative) a line's number of copies.
addLine :: LineNumber -> Count -> Whenever ()
addLine n c = count n >>= setCount n . (+ c)

-- | Gets the statement for a line, or error if the line number doesn't exist.
getStmt :: LineNumber -> Whenever Stmt
getStmt n = maybe err snd <$> gets (Map.lookup n)
  where err = error $ "getStmt: unknown line number " ++ show n

-- | Executes a line, and removes one copy of it. A 'Defer' clause means neither
-- of these things happen. An 'Again' clause means the statement executes, but
-- it is not removed.
runLine :: LineNumber -> Whenever ()
runLine n = getStmt n >>= go where
  go (Defer x s) = eval x >>= \b -> unless b $ go s
  go (Again x s) = eval x >>= \b -> if b then runStmt s else go s
  go (Commands ls) = runStmt (Commands ls) >> addLine n (-1)

-- | Executes a statement, without removing its line. A 'Defer' clause means the
-- statement isn't executed. An 'Again' clause is executed for its side-effects,
-- but does nothing, because the line won't be removed to begin with.
runStmt :: Stmt -> Whenever ()
runStmt (Defer x s) = eval x >>= \b -> unless b $ runStmt s
runStmt (Again x s) = eval x >> runStmt s
runStmt (Commands ls) = forM_ ls $ \(x, y) -> do
  n <- eval x
  c <- eval y
  case compare n 0 of
    LT -> addLine (abs n) (negate c)
    EQ -> return ()
    GT -> addLine n c

-- | Selects a random line number. Each line with at least one copy has an equal
-- chance of being selected. Returns Nothing if all lines have zero copies.
select :: Whenever (Maybe LineNumber)
select = do
  xs <- gets Map.toList
  let ns = [ n | (n, (c, _)) <- xs, c /= 0 ]
  case ns of
    [] -> return Nothing
    _ -> do
      i <- lift $ getStdRandom $ randomR (0, length ns - 1)
      return $ Just $ ns !! i

-- | Runs until no lines have a positive number of copies left.
run :: Whenever ()
run = select >>= maybe (return ()) (\n -> runLine n >> run)

-- | Assigns each line in a program to have 1 copy.
makeContext :: Program -> Context
makeContext p = Map.fromList [ (n, (1, s)) | (n, s) <- p ]

-- | Runs a program to completion.
runProgram :: Program -> IO ()
runProgram = evalStateT run . makeContext
