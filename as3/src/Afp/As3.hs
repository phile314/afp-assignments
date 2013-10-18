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
    esizedInt, erandomN,

    -- * Task 6.1
    preserves, Contract (..), assert, (-->), pos, true,
    
    preservesPos, preservesPos',
    -- $exc611
    
    allPos, allPos',

    -- * Task 8.4
    -- $exc84
    forceBoolList,

    -- * Task 8.5
    -- $exc85
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


eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil eqA Nil Nil = True

-- 4.3.1

-- | If the forall quantifier is omitted, an implicited forall quantifier outside the whole type expression is placed. The
--   type would loke something like this:
--
--   @
--   forall a b . (((b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Cons t a -> Cons t a -> Bool))
--   @
--
--   If the function is then called, the type variable b can only be instantiated with exactly one type,
--   but the passed lambda will be called on values of different types, therefor the type variable
--   would need to be instantiated with multiple types, which is not possible. Hence it can't work without the forall quantifier.
eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

-- 4.3.2

-- | It doesn't work when the forall quantifier is removed. The reason is exactly the same as for `eqCons`.
eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' eqT eqA  _         _        = False

eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)

-- 4.3.3

mapNil :: (a -> b) -> (Nil a -> Nil b)
mapNil fA Nil = Nil

mapCons :: (forall b . (b -> d) -> (t b -> t d)) -> (a -> d) -> (Cons t a -> Cons t d)
mapCons fT fA (Cons x xs) = Cons (fA x) (fT fA xs)

mapSquare' :: (forall b d . (b -> d) -> (t b -> t d)) -> (a -> d) -> (Square' t a -> Square' t d)
mapSquare' fT fA (Zero xs) = Zero $ fT (fT fA) xs
mapSquare' fT fA (Succ xs) = Succ $ mapSquare' (mapCons fT) fA xs

-- | Maps the given function over a `Square`.
mapSquare :: (a -> d) -> Square a -> Square d
mapSquare = mapSquare' mapNil

instance Functor Square where
  fmap = mapSquare

-- 5.2 (15%)
-- 
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
--
--
-- @
-- Ø            class A a
-- ------------------------
-- Ø ||- class A a      inst A a => A (Maybe a)         class A a       (A a) => B a
-- --------------------------------------------         ----------------------------
-- Ø ||- A a => A (Maybe a)                             inst (A a, B a) => A [a]
-- ---------------------------------------------------------------------------------[a <- (Maybe a)]
-- Ø ||-  A (Maybe [a])                                                                             inst A Bool
-- ------------------------------------------------------------------------------------------------------------- [a <- Bool]
-- Ø ||- A (Maybe [Bool]) 
-- @


-- 5.3 (15%)
one :: Int
one = 1
two :: Int
two = 2
randomN :: (RandomGen g) => Int -> g -> Int
randomN n g = (fst (next g) `mod` (two * n + one)) - n
-- | This is the most general type.
sizedInt :: (Monad m, MonadTrans t, MonadReader Int (t m), RandomGen g, MonadReader g m) => t m Int
sizedInt = do
  n <- ask
  g <- lift ask
  return (randomN n g)


-- Evidence Translation

data EMonad m = EMonad
  { ecomp1 :: forall a b . m a -> (a -> m b) -> m b
  , ereturn :: forall a . a -> m a }

data EMonadTrans t = EMonadTrans
  { elift :: forall a m . EMonad m -> m a -> t m a }

data EMonadReader r m = EMonadReader
  { eask :: m r
  , emonad :: EMonad m }

data ERandomGen g = ERandomGen
  { enext :: g -> (Int, g) }

-- | Class constraints are reintroduced in this definition to be able to use the std random generator. The class
-- constraint could be omitted if the random generator would be reimplemented here.
erandomgen_stdgen :: RandomGen g => ERandomGen g
erandomgen_stdgen = ERandomGen
  { enext = next }


newtype Identity a = Identity { runIdentity :: a }

emonad_id :: EMonad Identity
emonad_id = EMonad
  { ecomp1 = \f g -> g (runIdentity f)
  , ereturn = Identity }


erandomN :: ERandomGen g -> Int -> g -> Int
erandomN ergg n g = (fst ((enext ergg) g) `mod` (two * n + one)) - n

esizedInt :: EMonad m -> EMonadTrans t -> EMonadReader Int (t m) -> ERandomGen g -> EMonadReader g m -> t m Int
esizedInt emm emtt emritm ergg emrgm = s1 `cm1` s2
  where
    cm1 = ecomp1 (emonad emritm)
    cm2 = ecomp1 (emonad emritm)
    s1 = (eask emritm)
    s2 = \n -> (elift emtt) (emonad emrgm) (eask emrgm) `cm2` \g ->
        (ereturn (emonad emritm)) (erandomN ergg n g)
      

-- 6.1 (25%)
data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    Fun :: Contract a -> Contract b -> Contract (a -> b)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)
    List :: Contract a -> Contract [a]

assert :: Contract a -> a -> a
assert (Pred p)     x    = if p x then x else error "contract violation"
assert (Fun pre post) f  = assert post . f . assert pre
assert (DFun pre post) f = \x -> assert (post x) (f $ assert pre x) 
assert (List c) xs       = map (assert c) xs

-- | A contract which test is the number is positive
pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

-- | test the functionality of pos contract
test1 = assert pos 2 -- == 2
test2 = assert pos 0 -- == error

-- | A contract where true x == x holds.
true :: Contract a 
true = Pred (const True)

-- | A combinator that combines two contracts
(-->) :: Contract a -> Contract b -> Contract (a -> b)
x --> y = DFun x (const y)

-- | A contract suitable for the list index function (!!)
(!!) :: Contract ([a] -> Int -> a)
(!!) = DFun true (\xs -> DFun (Pred (check xs)) (\n -> Pred (\x -> True)))
    where 
        check :: [a] -> Int -> Bool
        check (x:_)  0 = True
        check []     _ = False
        check (_:xs) n = check xs (n - 1)

-- | preserves contract
preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves f = DFun true (preserve f)
    where
        preserve :: Eq b => (a -> b) -> a -> Contract a
        preserve f = (\l -> Pred ((==) l . f)) . f 

-- test functions for preserves
test3 = assert (preserves length) reverse "Hello"           -- "olleH"
test4 = assert (preserves length) (take 5) "Hello"          -- "Hello"
test5 = assert (preserves length) (take 5) "Hello world"    -- bottom

-- | preserves the positive number
preservesPos :: Contract (Integer -> Integer)
preservesPos = preserves (>0)
preservesPos' :: Contract (Integer -> Integer)
preservesPos' = pos --> pos

-- test functions for preservesPos
test6 = assert preservesPos (subtract 5) 5          -- = 0 which is an error violation
test7 = assert preservesPos' (subtract 5) 5         -- = 0 which is an error violation

test8 = assert preservesPos id (-5)             -- = -5 
test9 = assert preservesPos' id (-5)            -- = error violation

-- $exc611
--
-- There is no difference in the type of preservesPos and preservesPos'. The test above show that there is a difference in the impentation of the function. 
-- When appling the identity function to the function preservesPos you will not receive an error as the input is the same as the input. 
-- In the function preservesPos prime you will receive an error violation as the contact will be check before and after the identity function.



allPos = List pos
allPos' = Pred (all (>0))

test10 = assert allPos [1,2,-4,5]           -- = [1,2, error violation
test11 = assert allPos' [1,2,-4,5]          -- = error violation

{-  Like the two functions above of the preservesPos the test10 will check the contract when the element is needed to be printed due to lazy evaluation. The test11 shows that the contract will be tested on all the elements before the list would be printed. -}


-- 8.4 (10%)

-- $exc84
-- 
-- If the type of forceBoolList were specified as [Bool] -> [Bool], evaluation would not be forced until
-- the bool list were pattern matched on, because there is no reason to compute the result of the function
-- prior to that.
-- With the actual type in use here, a dependency between the first and second argument is created.
-- As soon as the return value of the function is pattern matched on,
-- evaluation of the first argument is enforced.
-- Exactly the same reasoning applies for `seq`.

-- | Evaluates the first argument to full normal form as soon as the return value is evaluated.
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

-- $exc85  For each the types will grow exponential. If you write out the types you will get f1 :: a -> T (T a) and f2 :: a -> T (T (T (T a))). The function f5 will have 32 T's. When that function is uncommented the compiler wouldn't complete any more in a reasonable time.

