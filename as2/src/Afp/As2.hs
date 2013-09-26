{-# LANGUAGE EmptyDataDecls #-}
module Afp.As2 (
  dummy,
  -- * Exercise 8.1
  -- $exc81
   
  -- * Exercise 9.1
  -- $exc91
) where

import Test.QuickCheck
import Control.Monad
import Control.Monad.Writer

dummy = 1



-- 2.7
--

type Object a = a -> a -> a
data X = X { n :: Int, f :: Int -> Int }

extendedBy :: Object a -> Object a -> Object a
extendedBy o1 o2 super this = o2 (o1 super this) this


fixObject o = o (error "super") (fixObject o)


zero :: Object a
zero super this = super

x, y, z :: Object X
x super this = X { n = 0, f = \i -> i + n this }
y super this = super { n = 1 }
z super this = super { f = f super . f super }


data Step a b = Enter a
  | Return b
  deriving Show

trace :: Object (a -> (b, Writer (Step Int Int) b))
{--trace super this arg = pass $ do
  tell (Enter 0)
  super arg
--}
trace = undefined


-- 2.9
--
p1, p2 :: Int

p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
--p3 = start store 2 add stop

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

