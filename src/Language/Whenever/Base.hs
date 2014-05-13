{-# LANGUAGE StandaloneDeriving, GADTs, OverloadedStrings #-}
module Language.Whenever.Base
( LineNumber
, Count
, Any(..)
, Expr(..)
, Stmt(..)
, Program
, Context
, WheneverIO(..)
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
, optimize
) where

import Control.Monad (forM_, unless, liftM, liftM2)
import Data.Char (isDigit)
import Data.Functor (void)
import Data.Monoid ((<>))
import System.IO (hLookAhead, stdin)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, evalRWST, ask, gets, modify)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (getStdRandom, randomR)

type LineNumber = Integer
type Count = Integer

data Any
  = Str T.Text
  | Int Integer
  | Bool Bool
  deriving (Eq, Ord, Show, Read)

getStr :: Any -> T.Text
getStr v = case v of
  Str  s -> s
  Int  i -> T.pack $ show i
  Bool b -> if b then "true" else "false"

getInt :: Any -> Integer
getInt v = case v of
  Str  s -> strToInt s
  Int  i -> i
  Bool b -> if b then 1 else 0

getBool :: (Monad m) => Any -> Whenever m Bool
getBool v = case v of
  Str  s -> intToBool $ strToInt s
  Int  i -> intToBool i
  Bool b -> return b

data Expr a where
  Val          :: a -> Expr a
  Append       :: Expr T.Text -> Expr T.Text -> Expr T.Text
  Add          :: Expr Integer -> Expr Integer -> Expr Integer
  Plus         :: Expr Any -> Expr Any -> Expr Any
  Sub          :: Expr Integer -> Expr Integer -> Expr Integer
  Mult         :: Expr Integer -> Expr Integer -> Expr Integer
  Div          :: Expr Integer -> Expr Integer -> Expr Integer
  Rem          :: Expr Integer -> Expr Integer -> Expr Integer
  Or           :: Expr Bool -> Expr Bool -> Expr Bool
  And          :: Expr Bool -> Expr Bool -> Expr Bool
  Compare      :: Ordering -> Expr Integer -> Expr Integer -> Expr Bool
  Equal        :: Expr Any -> Expr Any -> Expr Bool
  EqualStr     :: Expr T.Text -> Expr T.Text -> Expr Bool
  EqualBool    :: Expr Bool -> Expr Bool -> Expr Bool
  Not          :: Expr Bool -> Expr Bool
  Read         :: Expr Integer
  Print        :: [Expr T.Text] -> Expr Integer
  N            :: Expr Integer -> Expr Integer
  U            :: Expr Integer -> Expr T.Text
  If           :: Expr Bool -> Expr a -> Expr a -> Expr a
  IntToStr     :: Expr Integer -> Expr T.Text
  StrToInt     :: Expr T.Text -> Expr Integer
  BoolToInt    :: Expr Bool -> Expr Integer
  IntToBool    :: Expr Integer -> Expr Bool
  BoolToStr    :: Expr Bool -> Expr T.Text
  StrToBool    :: Expr T.Text -> Expr Bool
  AnyToStr     :: Expr Any -> Expr T.Text
  AnyToInt     :: Expr Any -> Expr Integer
  AnyToBool    :: Expr Any -> Expr Bool
  StrToAny     :: Expr T.Text -> Expr Any
  IntToAny     :: Expr Integer -> Expr Any
  BoolToAny    :: Expr Bool -> Expr Any

deriving instance (Eq a) => Eq (Expr a)
deriving instance (Show a) => Show (Expr a)

data Stmt
  = Defer (Expr Bool) Stmt
  | Again (Expr Bool) Stmt
  | Commands [(Expr Integer, Expr Integer)]
  deriving (Eq, Show)

type Program = [(LineNumber, Stmt)]
data WheneverIO m = WheneverIO
  { wPrint  :: T.Text -> m ()
  , wLook   :: m Char
  , wGet    :: m Char
  , wRandom :: Int -> m Int
  }
type Context = Map.Map LineNumber (Count, Stmt)
type Whenever m = RWST (WheneverIO m) () Context m

plus :: Any -> Any -> Any
plus (Str x) y       = Str $ x <> getStr y
plus x       (Str y) = Str $ getStr x <> y
plus x       y       = Int $ getInt x + getInt y

unwrapStr :: Expr Any -> Maybe (Expr T.Text)
unwrapStr (Val (Str s)) = Just (Val s)
unwrapStr (StrToAny e) = Just e
unwrapStr (If b t f) = liftM2 (If b) (unwrapStr t) (unwrapStr f)
unwrapStr _ = Nothing

unwrapInt :: Expr Any -> Maybe (Expr Integer)
unwrapInt (Val (Int i)) = Just (Val i)
unwrapInt (IntToAny e) = Just e
unwrapInt (If b t f) = liftM2 (If b) (unwrapInt t) (unwrapInt f)
unwrapInt _ = Nothing

unwrapBool :: Expr Any -> Maybe (Expr Bool)
unwrapBool (Val (Bool b)) = Just (Val b)
unwrapBool (BoolToAny e) = Just e
unwrapBool (If b t f) = liftM2 (If b) (unwrapBool t) (unwrapBool f)
unwrapBool _ = Nothing

-- | Evaluates an expression to return a value. This may have side-effects
-- (executing a 'Language.Whenever.Base.Read' or 'Print' expression) but won't
-- modify any line's count.
eval :: (Monad m) => Expr a -> Whenever m a
eval e = case e of
  Val          v   -> return v
  Append       x y -> liftM2 (<>) (eval x) (eval y)
  Add          x y -> liftM2 (+)  (eval x) (eval y)
  Plus         x y -> liftM2 plus (eval x) (eval y)
  Sub          x y -> liftM2 (-)  (eval x) (eval y)
  Mult         x y -> liftM2 (*)  (eval x) (eval y)
  Div          x y -> liftM2 quot (eval x) (eval y)
  Rem          x y -> liftM2 rem  (eval x) (eval y)
  Or           x y -> liftM2 (||) (eval x) (eval y)
  And          x y -> liftM2 (&&) (eval x) (eval y)
  Compare    o x y -> liftM2 (\a b -> compare a b == o) (eval x) (eval y)
  Equal        x y -> liftM2 (==) (eval x) (eval y)
  EqualStr     x y -> liftM2 (==) (eval x) (eval y)
  EqualBool    x y -> liftM2 (==) (eval x) (eval y)
  Not          x   -> not `liftM` eval x
  Read             -> readInput
  Print        xs  -> mapM eval xs >>= printAll >> return 0
  N            x   -> eval x >>= count
  U            x   -> (T.singleton . toEnum . fromIntegral) `liftM` eval x
  If         c t f -> eval c >>= \b -> eval $ if b then t else f
  IntToStr     x   -> (T.pack . show) `liftM` eval x
  StrToInt     x   -> strToInt `liftM` eval x
  BoolToInt    x   -> (fromIntegral . fromEnum) `liftM` eval x
  IntToBool    x   -> eval x >>= intToBool
  BoolToStr    x   -> (\b -> if b then "true" else "false") `liftM` eval x
  StrToBool    x   -> eval x >>= intToBool . strToInt
  AnyToStr     x   -> getStr `liftM` eval x
  AnyToInt     x   -> getInt `liftM` eval x
  AnyToBool    x   -> eval x >>= getBool
  StrToAny     x   -> Str `liftM` eval x
  IntToAny     x   -> Int `liftM` eval x
  BoolToAny    x   -> Bool `liftM` eval x

optimize :: Expr a -> Expr a
optimize expr = case expr of
  Val _ -> expr
  Append x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a <> b
    (Val "", y') -> y'
    (x', Val "") -> x'
    (x', y') -> Append x' y'
  Add x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a + b
    (Val 0, y') -> y'
    (x', Val 0) -> x'
    (x', y') -> Add x' y'
  Plus x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ plus a b
    (StrToAny x', y') -> StrToAny $ optimize $ Append x' (AnyToStr y')
    (x', StrToAny y') -> StrToAny $ optimize $ Append (AnyToStr x') y'
    (IntToAny x', IntToAny y') -> IntToAny $ optimize $ Add x' y'
    (IntToAny x', BoolToAny y') -> IntToAny $ optimize $ Add x' (BoolToInt y')
    (BoolToAny x', IntToAny y') -> IntToAny $ optimize $ Add (BoolToInt x') y'
    (BoolToAny x', BoolToAny y') -> IntToAny $ optimize $
      Add (BoolToInt x') (BoolToInt y')
    (x', y') -> Plus x' y'
  Sub x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a - b
    (x', Val 0) -> x'
    (x', y') -> Sub x' y'
  Mult x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a * b
    (Val 1, y') -> y'
    (x', Val 1) -> x'
    (x', y') -> Mult x' y'
  Div x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ quot a b
    (x', Val 1) -> x'
    (x', y') -> Div x' y'
  Rem x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ rem a b
    (_, Val 1) -> Val 0
    (x', y') -> Rem x' y'
  Or x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a || b
    (Val True, _) -> Val True
    (_, Val True) -> Val True
    (Val False, y') -> y'
    (x', Val False) -> x'
    (x', y') -> Or x' y'
  And x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a && b
    (Val False, _) -> Val False
    (_, Val False) -> Val False
    (Val True, y') -> y'
    (x', Val True) -> x'
    (x', y') -> And x' y'
  Compare o x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ compare a b == o
    (x', y') -> if o == EQ && x' == y'
      then Val True
      else Compare o x' y'
  Equal x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a == b
    (StrToAny x', y') -> optimize $ EqualStr x' (AnyToStr y')
    (x', StrToAny y') -> optimize $ EqualStr (AnyToStr x') y'
    (IntToAny x', IntToAny y') -> optimize $ Compare EQ x' y'
    (IntToAny x', BoolToAny y') -> optimize $ Compare EQ x' (BoolToInt y')
    (BoolToAny x', IntToAny y') -> optimize $ Compare EQ (BoolToInt x') y'
    (BoolToAny x', BoolToAny y') -> optimize $ EqualBool x' y'
    (x', y') -> if x' == y'
      then Val True
      else Equal x' y'
  EqualStr x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a == b
    (x', y') -> if x' == y'
      then Val True
      else EqualStr x' y'
  EqualBool x y -> case (optimize x, optimize y) of
    (Val a, Val b) -> Val $ a == b
    (x', y') -> if x' == y'
      then Val True
      else EqualBool x' y'
  Not x -> case optimize x of
    Val a -> Val $ not a
    x' -> Not x'
  Read -> Read
  Print xs -> let
    appends x = case x of
      Append a b -> appends a ++ appends b
      _ -> [x]
    in Print $ concatMap (appends . optimize) xs
  N x -> N $ optimize x
  U x -> case optimize x of
    Val a -> Val $ T.singleton $ toEnum $ fromIntegral a
    x' -> U x'
  If b t f -> case (optimize b, optimize t, optimize f) of
    (Val True, t', _) -> t'
    (Val False, _, f') -> f'
    (b', t', f') -> If b' t' f'
  IntToStr x -> case optimize x of
    Val i -> Val $ T.pack $ show i
    x' -> IntToStr x'
  StrToInt x -> case optimize x of
    Val s -> Val $ strToInt s
    IntToStr y -> y
    x' -> StrToInt x'
  BoolToInt x -> case optimize x of
    Val b -> Val $ if b then 1 else 0
    x' -> BoolToInt x'
  IntToBool x -> IntToBool $ optimize x
  BoolToStr x -> case optimize x of
    Val b -> Val $ if b then "true" else "false"
    x' -> BoolToStr x'
  StrToBool x -> case optimize x of
    Val s -> IntToBool $ Val $ strToInt s
    x' -> StrToBool x'
  AnyToStr x -> case optimize x of
    x' -> case (unwrapStr x', unwrapInt x', unwrapBool x') of
      (Just s, _, _) -> s
      (_, Just i, _) -> optimize $ IntToStr i
      (_, _, Just b) -> optimize $ BoolToStr b
      (Nothing, Nothing, Nothing) -> AnyToStr x'
  AnyToInt x -> case optimize x of
    x' -> case (unwrapStr x', unwrapInt x', unwrapBool x') of
      (Just s, _, _) -> optimize $ StrToInt s
      (_, Just i, _) -> i
      (_, _, Just b) -> optimize $ BoolToInt b
      (Nothing, Nothing, Nothing) -> AnyToInt x'
  AnyToBool x -> case optimize x of
    x' -> case (unwrapStr x', unwrapInt x', unwrapBool x') of
      (Just s, _, _) -> optimize $ StrToBool s
      (_, Just i, _) -> optimize $ IntToBool i
      (_, _, Just b) -> b
      (Nothing, Nothing, Nothing) -> AnyToBool x'
  StrToAny x -> StrToAny $ optimize x
  IntToAny x -> IntToAny $ optimize x
  BoolToAny x -> BoolToAny $ optimize x

strToInt :: T.Text -> Integer
strToInt s = case reads $ T.unpack s of
  [(i, _)] -> i
  _        -> 0

intToBool :: (Monad m) => Integer -> Whenever m Bool
intToBool i = (/= 0) `liftM` count i

printAll :: (Monad m) => [T.Text] -> Whenever m ()
printAll ts = do
  wio <- ask
  lift $ do
    mapM_ (wPrint wio) ts
    wPrint wio "\n"

-- | From standard input, reads either a natural number (sequence of digits),
-- or a single char (returning its codepoint) if it isn't a digit.
readInput :: (Monad m) => Whenever m Integer
readInput = do
  wio <- ask
  lift $ wLook wio >>= \c -> if isDigit c
    then let
      readWhile p = wLook wio >>= \x -> if p x
        then liftM2 (:) (wGet wio) $ readWhile p
        else return ""
      in read `liftM` readWhile isDigit
    else (fromIntegral . fromEnum) `liftM` wGet wio

-- | Returns the number of copies of a line.
count :: (Monad m) => LineNumber -> Whenever m Count
count n = maybe 0 fst `liftM` gets (Map.lookup n)

-- | Sets the number of copies of a line. A negative count sets to zero.
-- Setting to zero doesn't remove the line from memory; it might be readded
-- later.
setCount :: (Monad m) => LineNumber -> Count -> Whenever m ()
setCount n c = modify $ \m -> case Map.lookup n m of
  Just (_, x) -> Map.insert n (max 0 c, x) m
  Nothing     -> error $ "setCount: unknown line number " ++ show n

-- | Adds to (positive) or subtracts from (negative) a line's number of copies.
addLine :: (Monad m) => LineNumber -> Count -> Whenever m ()
addLine n c = count n >>= setCount n . (+ c)

-- | Gets the statement for a line, or error if the line number doesn't exist.
getStmt :: (Monad m) => LineNumber -> Whenever m Stmt
getStmt n = maybe err snd `liftM` gets (Map.lookup n)
  where err = error $ "getStmt: unknown line number " ++ show n

-- | Executes a line, and removes one copy of it. A 'Defer' clause means neither
-- of these things happen. An 'Again' clause means the statement executes, but
-- it is not removed.
runLine :: (Monad m) => LineNumber -> Whenever m ()
runLine n = getStmt n >>= go where
  go (Defer x s) = eval x >>= \b -> unless b $ go s
  go (Again x s) = eval x >>= \b -> if b then runStmt s else go s
  go (Commands ls) = runStmt (Commands ls) >> addLine n (-1)

-- | Executes a statement, without removing its line. A 'Defer' clause means the
-- statement isn't executed. An 'Again' clause is executed for its side-effects,
-- but does nothing, because the line won't be removed to begin with.
runStmt :: (Monad m) => Stmt -> Whenever m ()
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
select :: (Monad m) => Whenever m (Maybe LineNumber)
select = do
  xs <- gets Map.toList
  let ns = [ n | (n, (c, _)) <- xs, c /= 0 ]
  case ns of
    [] -> return Nothing
    _ -> do
      wio <- ask
      i <- lift $ wRandom wio $ length ns
      return $ Just $ ns !! i

-- | Runs until no lines have a positive number of copies left.
run :: (Monad m) => Whenever m ()
run = select >>= maybe (return ()) (\n -> runLine n >> run)

-- | Assigns each line in a program to have 1 copy.
makeContext :: Program -> Context
makeContext p = Map.fromList [ (n, (1, s)) | (n, s) <- p ]

standardIO :: WheneverIO IO
standardIO = WheneverIO
  { wPrint  = TIO.putStr
  , wLook   = hLookAhead stdin
  , wGet    = getChar
  , wRandom = \n -> getStdRandom $ randomR (0, n - 1)
  }

-- | Runs a program to completion.
runProgram :: Program -> IO ()
runProgram = void . evalRWST run standardIO . makeContext
