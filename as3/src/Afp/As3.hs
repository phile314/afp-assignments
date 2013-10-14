{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Afp.As3

where

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

-- ATTENTION: b and d have to be in the first forall quantifier
mapSquare' :: (forall b d . (b -> d) -> (t b -> t d)) -> (a -> d) -> (Square' t a -> Square' t d)
mapSquare' fT fA (Zero xs) = Zero $ fT (fT fA) xs
mapSquare' fT fA (Succ xs) = Succ $ mapSquare' (mapCons fT) fA xs

mapSquare :: (a -> d) -> Square a -> Square d
mapSquare = mapSquare' mapNil

instance Functor Square where
  fmap = mapSquare

-- 5.2 (15%)
-- ph

-- 5.3 (15%)
--

-- 6.1 (25%)
-- laurens

-- 8.4 (10%)
-- ph; finished - unchecked

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
-- TODO
--

