{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Afp.As3
  ( -- * Task 4.3
    Square, Square', Nil, eye2, m2, eqNil, eqCons, eqSquare', eqSquare, mapSquare,

    -- * Task 5.2
    -- $exc52

    -- * Task 5.3
    one, two, randomN, sizedInt,

    -- * Task 6.1
    preserves, Contract (..),

    -- * Task 8.4
    -- $exc84
    forceBoolList,

    -- * Task 8.5
    T, T2, T4, T8, T16, f0, f1, f2, f3, f4)
where

import Control.Monad.Reader
import System.Random

-- 4.3 (25%)
--
type Square = Square' Nil
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a = Nil
data Cons t a = Cons a (t a)

eye2 = Succ (Succ (Zero (Cons (Cons 1 (Cons 0 Nil))(Cons (Cons 0 (Cons 1 Nil))Nil))))
m2 = Succ (Succ (Succ (Zero (Cons (Cons 1 (Cons 2 (Cons 3 Nil)))(Cons (Cons 4 (Cons 5 (Cons 6 Nil)))(Cons (Cons 7 (Cons 8 (Cons 9 Nil)))Nil))))))


-- 4.3.1
-- A type with a (forall) on the inside requires the extension RankNTypes to be enabled.
-- Try to understand what the difference is between a function of the type of eqCons
-- and a function with the same type but the (forall) omitted. Can you omit
-- the (forall) in the case of eqCons and does the function still work?
eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil eqA Nil Nil = True

-- | 4.3.1 If the forall quantifier is omitted, an implicited forall quantifier outside the whole type expression is placed. The
--   type would loke something like this:
--   forall a b . (((b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Cons t a -> Cons t a -> Bool))
--   TODO what's the problem? oo
eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

-- 4.3.2
-- Task.
-- Again, try removing the (forall) from the type of eqSquare'.
-- Does the function still typecheck? Try to explain!

-- | 4.3.2 TODO doesn't work without forall, but why?
eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' eqT eqA  _         _        = False

eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)

-- 4.3.3
-- Systematically follow the scheme just presented in order to define a Functor instance
-- for square matrices. I.e., derive a function mapSquare such that you can define

-- | 4.3.3
-- ph - finished - unchecked
mapNil :: (a -> b) -> (Nil a -> Nil b)
mapNil fA Nil = Nil

mapCons :: (forall b . (b -> d) -> (t b -> t d)) -> (a -> d) -> (Cons t a -> Cons t d)
mapCons fT fA (Cons x xs) = Cons (fA x) (fT fA xs)

-- ATTENTION: b and d have to be in the forall quantifier
mapSquare' :: (forall b d . (b -> d) -> (t b -> t d)) -> (a -> d) -> (Square' t a -> Square' t d)
mapSquare' fT fA (Zero xs) = Zero $ fT (fT fA) xs
mapSquare' fT fA (Succ xs) = Succ $ mapSquare' (mapCons fT) fA xs

-- | Maps the given function over a `Square`.
mapSquare :: (a -> d) -> Square a -> Square d
mapSquare = mapSquare' mapNil

instance Functor Square where
  fmap = mapSquare

-- 5.2 (15%)
-- ph

-- 5.3 (15%)
one :: Int
one = 1
two :: Int
two = 2
randomN :: (RandomGen g) => Int -> g -> Int
randomN n g = (fst (next g) `mod` (two * n + one)) - n
-- | This is the most general type.
--   TODO evidence translation!!!
sizedInt :: (Monad m, MonadTrans t, MonadReader Int (t m), RandomGen g, MonadReader g m) => t m Int
sizedInt = do
  n <- ask
  g <- lift ask
  return (randomN n g)

-- TODO 5.3 evidence translation


-- 6.1 (25%)
-- laurens
data Contract :: * -> * where
	Pred :: (a -> Bool) -> Contract a
	Fun :: Contract a -> Contract b -> Contract (a -> b)
	DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)
	List :: Contract a -> Contract [a]

assert :: Contract a -> a -> a
assert (Pred p) 	x 	 = if p x then x else error "contract violation"
assert (Fun pre post) f  = assert post . f . assert pre
assert (DFun pre post) f = \x -> assert (post x) (f $ assert pre x) 
assert (List c) xs 		 = map (assert c) xs

-- ! A contract which test is the number is positive
pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

-- ! test the functionality of pos contract
test1 = assert pos 2 -- == 2
test2 = assert pos 0 -- == error

-- ! A contract where true x == x holds.
true :: Contract a 
true = Pred (const True)

-- ! A combinator that combines two contracts
(-->) :: Contract a -> Contract b -> Contract (a -> b)
x --> y = DFun x (const y)

-- ! A contract suitable for the list index function (!!)
(!!) :: Contract ([a] -> Int -> a)
(!!) = DFun true (\xs -> DFun (Pred (check xs)) (\n -> Pred (\x -> True)))
	where 
		check :: [a] -> Int -> Bool
		check (x:_)	 0 = True
		check [] 	 _ = False
		check (_:xs) n = check xs (n - 1)

-- ! preserves contract
preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves f = DFun true (preserve f)
	where
		preserve :: Eq b => (a -> b) -> a -> Contract a
		preserve f = (\l -> Pred ((==) l . f)) . f 

-- ! test functions for preserves
test3 = assert (preserves length) reverse "Hello" 			-- "olleH"
test4 = assert (preserves length) (take 5) "Hello"	 		-- "Hello"
test5 = assert (preserves length) (take 5) "Hello world" 	-- bottom

-- ! preserves the positive number
preservesPos :: Contract (Integer -> Integer)
preservesPos = preserves (>0)
preservesPos' :: Contract (Integer -> Integer)
preservesPos' = pos --> pos

-- ! test functions for preservesPos
test6 = assert preservesPos (subtract 5) 5 			-- = 0 which is an error violation
test7 = assert preservesPos' (subtract 5) 5 		-- = 0 which is an error violation

test8 = assert preservesPos id (-5) 			-- = -5 
test9 = assert preservesPos' id (-5) 			-- = error violation

{-	There is no difference in the type of preservesPos and preservesPos'. The test above show that there is a difference in the impentation of the function. When appling the identity function to the function preservesPos you will not receive an error as the input is the same as the input. In the function preservesPos prime you will receive an error violation as the contact will be check before and after the identity function.
-}

allPos = List pos
allPos' = Pred (all (>0))

test10 = assert allPos [1,2,-4,5]			-- = [1,2, error violation
test11 = assert allPos' [1,2,-4,5] 			-- = error violation

{-	Like the two functions above of the preservesPos the test10 will check the contract when the element is needed to be printed due to lazy evaluation. The test11 shows that the contract will be tested on all the elements before the list would be printed. 

-}

-- 8.4 (10%)
-- ph; UNCHECKED

-- $exc84
-- 
-- If the type of forceBoolList were specified as [Bool] -> [Bool], evaluation would not be forced until the actual
-- return value of forceBoolList were pattern matched on.
-- With the actual type in use here, a dependency between the first and second argument is created.
-- As soon as the return value of the function is pattern matched on,
-- evaluation of the first argument is enforced.
-- Exactly the same reasoning applies for `seq`.

-- | Evaluates the first argument to full normal form as soon as the return value is used.
forceBoolList :: [Bool] -> r -> r
forceBoolList (True:xs) r = forceBoolList xs r
forceBoolList (False:xs)r = forceBoolList xs r
forceBoolList []        r = r



-- 8.5 (10%)
--
type T   a = (a, a)
type T2  a = T  (T a)
type T4  a = T2 (T2 a)
type T8  a = T4 (T4 a)
type T16 a = T8 (T8 a)

f0 :: a -> T a
f1 :: a -> T2 a
f2 :: a -> T4 a
f3 :: a -> T8 a
f4 :: a -> T16 a

f0 x = (x,x)
f1   = f0 . f0
f2   = f1 . f1
f3   = f2 . f2
f4   = f3 . f3
-- f5   = f4 . f4

{-	For each the types will grow exponential. If you write out the types you will get f1 :: a -> T (T a) and f2 :: a -> T (T (T (T a))). The function f5 will have 32 T's. When that function is uncommented the compiler wouldn't complete any more in a reasonable time.
-}

-- $exc52
--
-- @
-- cls (A a) => B a
-- ----------------
-- B a ||- A a           inst A a => A ( Maybe a)    inst A   a         => A ( Maybe a)
-- ----------------------------------------------    --------------------------------------------- [a <- (Maybe a)]
-- B a   ||- A ( Maybe a)                            inst A ( Maybe a ) => A ( Maybe ( Maybe a ) )
-- -----------------------------------------------------------------------------------------------
-- B a   ||- A ( Maybe ( Maybe a))
-- --------------------------------- [a <- Int]
-- B Int ||- A ( Maybe ( Maybe Int))
-- @
