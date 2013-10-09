{-# LANGUAGE RankNTypes #-}

module Afp.As3

where

-- 4.3 (25%)
--
type Square = Square' Nil
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a = Nil
data Cons t a = Cons a (t a)

-- 4.3.1
-- A type with a (forall) on the inside requires the extension RankNTypes to be enabled.
-- Try to understand what the difference is between a function of the type of eqCons
-- and a function with the same type but the (forall) omitted. Can you omit
-- the (forall) in the case of eqCons and does the function still work?

-- 4.3.2
-- Task.
-- Again, try removing the (forall) from the type of eqSquare'.
-- Does the function still typecheck? Try to explain!

-- 4.3.3
-- Systematically follow the scheme just presented in order to define a Functor instance
-- for square matrices. I.e., derive a function mapSquare such that you can define

-- instance Functor Square where
--   fmap = mapSquare

-- 5.2 (15%)
-- ph

-- 5.3 (15%)
--

-- 6.1 (25%)
-- laurens

-- 8.4 (10%)
-- ph

forceBoolList :: [Bool] -> r -> r
forceBoolList (True:xs) r = forceBoolList xs r
forceBoolList (False:xs)r = forceBoolList xs r
forceBoolList []        r = r

-- if the type were specified as [Bool] -> [Bool], evaluation would not be forced until the actual
-- return value of forceBoolList were pattern matched on.
-- With the actual type in use here, a dependency between the first and second argument is created.
-- As soon as the return value of the function is pattern matched on,
-- evaluation of the first argument is enforced.
-- Exactly the same reasoning applies for `seq`.



-- 8.5 (10%)
