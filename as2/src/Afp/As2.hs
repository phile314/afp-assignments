{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
module Afp.As2 (
  extendedBy, calls, trace, fac, fixObject, Object,

  start, stop, store, add, mul,
  -- * Exercise 8.1
  -- $exc81
   
  -- * Exercise 9.1
  -- $exc91
) where

import Test.QuickCheck
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State


-- 2.7
--

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


-- Solution Task 2.7.1

zero :: Object a
zero super this = super

-- Solution Task 2.7.2

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
--p3 = start store 2 add stop


-- Solution Task 2.9
data Zero
data Succ a

--This doesn't work with the "type" keyword.
data Stack a = St [Int]

type Cont a b = Stack a -> b

start :: (Stack Zero -> a -> b) -> a -> b
start = (\c -> c (St []))
stop :: Stack (Succ Zero) -> Int
stop (St [x]) = x

store :: Stack b -> Int -> Cont (Succ b) a -> a
store (St ss) x = \c -> c (St (x:ss))

add :: Stack (Succ (Succ b)) -> Cont (Succ b) a -> a
add (St (s1:s2:st)) = \c -> c (St ((s1 + s2):st))

mul :: Stack (Succ (Succ b)) -> Cont (Succ b) a -> a
mul (St (s1:s2:st)) = \c -> c (St ((s1 * s2):st))


-- 3
--
data StateMonadPlus s a = StM (s -> (a, s))

instance Monad (StateMonadPlus s) where  
    return x = StM $ \s -> (x,s)  
    (StM h) >>= f = StM $ \s -> 
        let (a, newState) = h s  
        in  let (StM g) = f a
			in g newState  

diagnostics :: StateMonadPlus s String
diagnostics = undefined


annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate = undefined

m :: StateMonadPlus Int String
m = 
	do 	return 3 >> return 4
		return 5
		diagnostics

-- 4.1
--

-- data F a = F { unF :: F a -> a }
--y = \f -> (\x -> f (x x)) (\x -> f (x x))

data F a = F { unF :: F a -> a }

y' = \f -> (\x -> f ((unF x) x)) (F (\x -> f ((unF x) x)))


-- 4.2
--

-- Matrices are specified in row-major order.
-- TODO: somebody should check the definitions

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


m2 = Succ (Succ (Succ (Zero (Cons (Cons 1 (Cons 2 (Cons 3 Nil)))(Cons (Cons 4 (Cons 5 (Cons 6 Nil)))(Cons (Cons 7 (Cons 8 (Cons 9 Nil)))Nil))))))
