{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Afp.As2 (
  -- * Task 2.7
  extendedBy, calls, fac, fixObject, Object,
  Step, zero, trace,

  -- * Task 2.9
  start, stop, store, add, mul, nop,
  Stack, Cont, Zero1, Succ1,

  -- * Task 3
  StateMonadPlus, Entry, 
  diagnostics, annotate,
  evalStatePlus,
  StoreState(..),

  -- * Task 4.1
  y',

  -- * Task 4.2
  eye2,
  m2,
  Nil,
  Square'
) where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error


-- 2.7
--

-- Definitions from the assignment

type Object a = a -> a -> a
data X = X { n :: Int, f :: Int -> Int }

extendedBy :: Object a -> Object a -> Object a
extendedBy o1 o2 super this = o2 (o1 super this) this


fixObject o = o (error "super") (fixObject o)


x, y, z :: Object X
x super this = X { n = 0, f = \i -> i + n this }
y super this = super { n = 1 }
z super this = super { f = f super . f super }

fac :: Monad m => Object (Int -> m Int)
fac super this n =
  case n of
    0 -> return 1
    n -> liftM (n*) (this (n - 1))

calls :: MonadState Int m => Object (a -> m b)
calls super this n =
  do
    modify (+1)
    super n


data Step a b = Enter a
  | Return b
  deriving Show


-- | Task 2.7.1. The identity object. 
zero :: Object a
zero super this = super

-- | Task 2.7.2. Traces all calls to functions of the super object.
trace :: MonadWriter [Step a b] m => Object (a -> m b)
trace super this arg = do
  tell [Enter arg]
  res <- super arg
  tell [Return res]
  return res


-- 2.9
--
p1, p2 :: Int

p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
-- p3 = start store 2 add stop



-- We could use given empty data decls here, but haddock fails for some reason if we use them.
-- | Type-level zero.
data Zero1 = Zero1'
-- | Type-level successor.
data Succ1 a = Succ1'

--This doesn't work with the "type" keyword.
-- | The stack. The size of the stack is given as a phantom type. The use of a phantom type
-- allows the use of a standard list, at the cost of allowing wrong functions to be defined.
-- E.g. the type of the first argument of `add` could be changed to (Stack (Succ b)),
-- without producing any compile time errors but introducing the risk of runtime
-- errors. Therefor, it is NOT advised to define any \"wrongly\" typed functions.
-- Defining a list which contains a type-level counter and enforces that it contains the right number of elements would solve this problem.
data Stack a = St [Int]

-- | A continuation.
type Cont a b = Stack a -> b

-- | Marks the start of a program.
start :: (Stack Zero1 -> a -> b) -> a -> b
start = (\c -> c (St []))
-- | Marks the end of a program and returns the top stack element. Enforces that the stack only has one element.
stop :: Stack (Succ1 Zero1) -> Int
stop (St [x]) = x

-- | Stores the given value on the stack.
store :: Stack b -> Int -> Cont (Succ1 b) a -> a
store (St ss) x = \c -> c (St (x:ss))

-- | Pops and adds the top two stack elements and pushes the result onto the stack.
add :: Stack (Succ1 (Succ1 b)) -> Cont (Succ1 b) a -> a
add (St (s1:s2:st)) = \c -> c (St ((s1 + s2):st))

-- | Pops and multiplies the top two stack elements and pushes the result onto the stack.
mul :: Stack (Succ1 (Succ1 b)) -> Cont (Succ1 b) a -> a
mul (St (s1:s2:st)) = \c -> c (St ((s1 * s2):st))

-- | Does nothing.
nop :: Stack a -> Cont a b -> b
nop s = \c -> c s

-- 3
--

-- Datatypes to track the state alongside of our state
type MyState = [Entry]
data Entry = Entry (String, Int) | Store [Int]

instance Show Entry where
	show (Entry (s, i)) = s ++ "=" ++ show i
	show (Store s) 		= "store=" ++ show s

-- Custom datatype for State Monad
-- | The StateMonadPlus provides a state in alongside of the MyState
data StateMonadPlus s a = SMP (State (MyState, s) a)

instance Monad (StateMonadPlus s) where  
    return x = SMP (state $ \(m,s) -> (x, (addReturn m,s)))
    (SMP x) >>= y  = SMP $ state $ \(m, s) -> 
		let (a, (m', s')) = runState x (m, s)
        in (\(SMP t) -> runState t (addBind m', s')) (y a)

instance MonadState s (StateMonadPlus s) where
    get   = SMP $ state $ \(m,s) -> (s, (addGet m,s))
    put s = SMP $ state $ \(m,_) -> ((), (addPut m,s))


-- update in the custom state that return is called
addReturn :: MyState -> MyState
addReturn = update "return"

-- update in the custom state that put is called
addPut :: MyState -> MyState
addPut = update "put"

-- update in the custom state that get is called
addGet :: MyState -> MyState
addGet = update "get"

-- update in the custom state that bind is called
addBind :: MyState -> MyState
addBind = update "bind" 

-- update in the custom state that diagnostics is called
addDiagnostics :: MyState -> MyState
addDiagnostics = update "diagnostics"

-- update in the custom state that annotate is called
-- update in one go the annotate and the annotation
addAnnotate :: String -> MyState -> MyState
addAnnotate = update False
	where
		update _ a [] 									= [Entry ("annotate", 1), Entry (a, 1)]
		update b a (Entry (s, i):xs)| s == "annotate" 	= Entry (s, i+1) : (if b then xs else update True a xs)
									| s == a 			= Entry (s, i+1) : (if b then xs else update True a xs)
									| otherwise			= Entry (s, i) : update b a xs
		update b a (x:xs)								= x: update b a xs

-- update an entry in the custom state
update :: String -> MyState -> MyState
update a = updateEntry a (1+)

updateEntry :: String -> (Int -> Int) -> MyState -> MyState
updateEntry a _ [] 					= [Entry (a, 1)]
updateEntry a f (Entry (k,v):xs)
	| k == a						= Entry (k, f v) : xs
	| otherwise						= Entry (k, v) : updateEntry a f xs
updateEntry a f (x:xs)				= x : updateEntry a f xs


-- | The function diagnostics substitutes the value of the state with the output of the custom state
diagnostics :: StateMonadPlus s String
diagnostics = SMP $ state $ \(m,s) -> ((show $ addDiagnostics m), (addDiagnostics m,s))


-- | The function annotate let you update a value into the custom state and continues with the next execution
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate a (SMP x) = SMP $ state $ \(m,s) -> runState x (addAnnotate a m, s)

-- monadDiagnostics is the function provided in the exercise for testing the diagnostics function
monadDiagnostics :: StateMonadPlus Int String
monadDiagnostics = 
	do 	return 3 >> return 4
		return 5
		diagnostics

-- monadAnnotate is the function provided in the exercise for testing the annotate function
monadAnnotate :: StateMonadPlus Int String
monadAnnotate =
	do	annotate "A" (return 3 >> return 4)
		return 5
		diagnostics

-- let you evalute a StateMonadPlus provided a state
evalStatePlus :: StateMonadPlus s a -> (MyState, s) -> a
evalStatePlus (SMP act) = fst . runState act

-- initalization of a state for StateMonadPlus
initState :: (MyState, Int)
initState = ([], 1)

-- function to test the functionality
testDiagnostics = print $ evalStatePlus monadDiagnostics initState
testAnnotate = print $ evalStatePlus monadAnnotate initState


--
class MonadState s m => StoreState s m | m -> s where
	saveState :: m ()
	loadState :: m ()

instance StoreState Int (StateMonadPlus Int) where
	saveState = SMP $ state $ \(m,s) -> ((), (pushStack s m, s))
	loadState = SMP $ state $ \(m,s) -> case catchError (popStack m) fail of
		(Right (i, m'))-> ((), (m',i))

-- failing function uses the functionality from the error function
failing :: (Monad m) => String -> m a
failing = error

-- push a value on the stack inside the custom state
pushStack :: Int -> MyState -> MyState
pushStack i [] = [Store [i]]
pushStack i (Store s:xs) = Store (i:s):xs
pushStack i (x:xs) = x : pushStack i xs

-- pops a value from the stack inside the custom state
-- returns the value and the changed state 
popStack :: MyState -> Either String (Int, MyState)
popStack = pop []
	where 
		pop _ []				= failing "failed to pop from the stack"
		pop _ (Store []:_) 		= failing "failed to pop from the stack"
		pop m (Store (y:ys):xs)	= Right (y, m ++ (Store ys : xs))
		pop m (x:xs) 			= pop (x:m) xs

-- tests the functionality of the StoreState monad
testStore = print $ evalStatePlus test initState
	where test = 
			do	i1 <- get; saveState
				modify (*2)
				i2 <- get; saveState
				modify (*2)
				i3 <- get; loadState
				i4 <- get; loadState
				i5 <- get
				return (i1,i2,i3,i4,i5)

-- tests the functionality of the StoreState monad
-- the test is designed to fail, because it will try to load a value from a empty stack
testStoreFail = print $ evalStatePlus test initState
	where test = 
			do	i1 <- get; loadState
				modify (*2)	
				i2 <- get; saveState
				modify (*2)
				i3 <- get; loadState
				i4 <- get; loadState
				i5 <- get
				return (i1,i2,i3,i4,i5)


-- 4.1
--

-- data F a = F { unF :: F a -> a }
--y = \f -> (\x -> f (x x)) (\x -> f (x x))

data F a = F { unF :: F a -> a }

-- | Works with ghci, but ghc 7.4.2 doesn't terminate when compiling this code for some reason. 
y' = \f -> (\x -> f ((unF x) x)) (F (\x -> f ((unF x) x)))


-- 4.2
--

-- Matrices are specified in row-major order.
type Square a    = Square' Nil a
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a       = Nil
data Cons t a    = Cons a (t a)

{--
Succ (
    Succ (
        Zero (
            Cons (
                Cons 1 (
                    Cons 0 Nil
                )
            )
            (
                Cons (
                    Cons 0 (
                        Cons 1 Nil
                    )
                    
                )
                Nil
            )
        )
    )
)
--}
-- | Task 4.2.1. The 2-dimensional identity matrix.
eye2 = Succ (Succ (Zero (Cons (Cons 1 (Cons 0 Nil))(Cons (Cons 0 (Cons 1 Nil))Nil))))


{--
Succ (
    Succ (
        Succ (
            Zero (
                Cons (
                    Cons 1 (
                        Cons 2 (
                            Cons 3 Nil
                        )
                    )
                )
                (
                    Cons (
                        Cons 4 (
                            Cons 5 (
                                Cons 6 Nil
                            )
                        )
                    
                    )
                    (
                        Cons (
                            Cons 7 (
                                Cons 8 (
                                    Cons 9 Nil
                                )
                            )
                        )
                        Nil
                    )
                )
            )
        )
    )
)
--}

-- | Task 4.2.2. (Row-major order)
m2 = Succ (Succ (Succ (Zero (Cons (Cons 1 (Cons 2 (Cons 3 Nil)))(Cons (Cons 4 (Cons 5 (Cons 6 Nil)))(Cons (Cons 7 (Cons 8 (Cons 9 Nil)))Nil))))))
