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
p1, p2, p3 :: Int

p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
p3 = start store 2 add stop

--data Nat a = Zero | Succ a

type Stack = [Int]

type Cont a = Stack -> a -> Stack


start :: (Stack -> a -> b) -> a -> b
start = (\c -> c [])
stop :: Stack -> Int
stop [x] = x

store :: Stack -> Int -> ((Stack -> a) -> a)
store st x = \c -> c (x:st)

add :: Stack -> (Stack -> a) -> a
add (s1:s2:st) = \c -> c ((s1 + s2):st)

mul :: Stack -> (Stack -> a) -> a
mul (s1:s2:st) = \c -> c ((s1 * s2):st)

