module Language.Whenever.Base where

import qualified Data.Map as Map
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Monad
import System.Random
import System.IO
import Data.Char (isDigit)

type LineNumber = Integer
type Count = Integer

data Val
  = Str String
  | Int Integer
  | Bool Bool
  deriving (Eq, Ord, Show, Read)

data Expr
  = Val Val
  | Binop Binop Expr Expr
  | Not Expr
  -- ^ 'Language.Whenever.Base.Bool' negation
  | Read
  -- ^ Uses 'readInput' to read a natural number, or a single character
  | Print Expr
  -- ^ Prints a 'Str' and a newline
  | N Expr
  -- ^ Returns the number of copies of a line number
  | U Expr
  -- ^ 'Language.Whenever.Base.Int', a codepoint, to a one-character 'Str'
  | If Expr Expr Expr
  -- ^ Ternary conditional operator
  deriving (Eq, Ord, Show, Read)

data Binop
  = Plus         -- ^ String concatenation or addition
  | Sub
  | Mult
  | Div
  | Or           -- ^ Short-circuit boolean operator
  | And          -- ^ Short-circuit boolean operator
  | Less         -- ^ 'Language.Whenever.Base.Int' comparison
  | Greater      -- ^ 'Language.Whenever.Base.Int' comparison
  | LessEqual    -- ^ 'Language.Whenever.Base.Int' comparison
  | GreaterEqual -- ^ 'Language.Whenever.Base.Int' comparison
  | Equal        -- ^ Exactly equal, or equal after 'getInt'
  | NotEqual     -- ^ Not exactly equal, and not equal after 'getInt'
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Stmt
  = Defer Expr Stmt
  | Again Expr Stmt
  | Commands [(Expr, Expr)]
  deriving (Eq, Ord, Show, Read)

type Program = [(LineNumber, Stmt)]
type Context = Map.Map LineNumber (Count, Stmt)
type Whenever = StateT Context IO

-- | Evaluates an expression to return a value. This may have side-effects
-- (executing a 'Language.Whenever.Base.Read' or 'Print' expression) but won't
-- modify any line's count.
eval :: Expr -> Whenever Val
eval e = case e of
  Val v -> return v
  Binop op x y -> case op of
    Plus         -> liftA2 plus (eval x) (eval y)
    Sub          -> Int  <$> liftA2 (-)  (evalInt  x) (evalInt  y)
    Mult         -> Int  <$> liftA2 (*)  (evalInt  x) (evalInt  y)
    Div          -> Int  <$> liftA2 quot (evalInt  x) (evalInt  y)
    Or           -> evalBool x >>= \b ->
      Bool <$> if b then return True else evalBool y
    And          -> evalBool x >>= \b ->
      Bool <$> if b then evalBool y else return False
    Less         -> Bool <$> liftA2 (<)  (evalInt  x) (evalInt  y)
    Greater      -> Bool <$> liftA2 (>)  (evalInt  x) (evalInt  y)
    LessEqual    -> Bool <$> liftA2 (<=) (evalInt  x) (evalInt  y)
    GreaterEqual -> Bool <$> liftA2 (>=) (evalInt  x) (evalInt  y)
    Equal        -> Bool <$> liftA2 equal (eval x) (eval y)
    NotEqual     -> Bool . not <$> liftA2 equal (eval x) (eval y)
  Not x -> Bool . not <$> evalBool x
  Read -> Int <$> readInput
  Print x -> evalStr x >>= lift . putStrLn >> return (Int 0)
  N x -> fmap Int $ evalInt x >>= count
  U x -> Str . (:[]) . toEnum . fromIntegral <$> evalInt x
  If c t f -> evalBool c >>= \b -> eval $ if b then t else f

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

-- | String concatenation (if either arg is a 'Str') or addition.
plus :: Val -> Val -> Val
plus (Str x) y       = Str $ x ++ getStr y
plus x       (Str y) = Str $ getStr x ++ y
plus x       y       = Int $ getInt x + getInt y

-- | True if two values are exactly equal, or if they are equal when converted
-- with 'getInt'.
equal :: Val -> Val -> Bool
equal x y = x == y || getInt x == getInt y

getStr :: Val -> String
getStr (Str  s) = s
getStr (Int  i) = show i
getStr (Bool b) = if b then "true" else "false"

-- | A string is converted to the integer it starts with, or 0 if there is none.
getInt :: Val -> Integer
getInt (Str  s) = case reads s of
  [(i, _)] -> i
  _        -> 0
getInt (Int  i) = i
getInt (Bool b) = if b then 1 else 0

-- | An 'Language.Whenever.Base.Int' or 'Str' is converted by first applying
-- 'getInt', and then checking if there are any copies of that numbered line.
getBool :: Val -> Whenever Bool
getBool (Bool b) = return b
getBool v = (/= 0) <$> count (getInt v)

evalStr :: Expr -> Whenever String
evalStr = fmap getStr . eval

evalInt :: Expr -> Whenever Integer
evalInt = fmap getInt . eval

evalBool :: Expr -> Whenever Bool
evalBool = eval >=> getBool

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
  go (Defer x s) = evalBool x >>= \b -> if b then return () else go s
  go (Again x s) = evalBool x >>= \b -> if b then runStmt s else go s
  go (Commands ls) = runStmt (Commands ls) >> addLine n (-1)

-- | Executes a statement, without removing its line. A 'Defer' clause means the
-- statement isn't executed. An 'Again' clause is executed for its side-effects,
-- but does nothing, because the line won't be removed to begin with.
runStmt :: Stmt -> Whenever ()
runStmt (Defer x s) = evalBool x >>= \b -> if b then return () else runStmt s
runStmt (Again x s) = evalBool x >> runStmt s
runStmt (Commands ls) = forM_ ls $ \(x, y) -> do
  n <- evalInt x
  c <- evalInt y
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

run :: Whenever ()
run = select >>= maybe (return ()) (\n -> runLine n >> run)

makeContext :: Program -> Context
makeContext p = Map.fromList [ (n, (1, s)) | (n, s) <- p ]

runProgram :: Program -> IO ()
runProgram = evalStateT run . makeContext
