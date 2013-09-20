module Afp.As1 (
  test, test', count, count', Countable, smooth_perms, smooth_perms_fast,
  -- * Exercise 9.1
  -- $exc91
) where

import Test.QuickCheck

-- Assignment 1.1
-- Cabalize everything
-- what should we put in the package name?


-- Assignment 2.5

-- Should we remove the links??
-- Creating a varargs function in Haskell is explained on http://www.haskell.org/haskellwiki/Varargs
-- There is another, different version on http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn ,
-- but it uses language extensions.

-- | The test case from the assignment.
test ::  [Int]
test =   [count, count 1 2 3, count "" [True, False] id (+)]
test' :: [Int]
test' =  [count', count' 1 2 3, count' "" [True, False] id (+)]

class Countable b where
  counti :: Int -> b
  counti' :: Int -> b

instance Countable Int where
  counti _ = 0
  counti' = id

instance Countable b => Countable (a -> b) where
  counti _ = \x -> counti 0
  counti' acc = \x -> counti' (acc + 1)

-- | Consumes all available arguments and returns zero.
count :: Countable a => a
count = counti 0

-- | Returns the number of arguments given.
count' :: Countable a => a
count' = counti' 0


-- Assignment 7.1
split :: [a] -> [(a, [a])]
split [] 		= []
split (x:xs) 	= (x, xs) : [(y, x:ys) | (y, ys) <- split xs]

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [(v:p) | (v,vs) <- split xs, p <- perms vs]

smooth :: (Ord a, Num a) => a -> [a] -> Bool
smooth n (x:y:ys) 	= abs (y - x) <= n && smooth n (y:ys)
smooth _ _ 			= True

smooth_perms :: Int -> [Int] -> [[Int]]
smooth_perms n xs 	= filter (smooth n) (perms xs)

smooth_perms_fast :: Int -> [Int] -> [[Int]]
smooth_perms_fast n = perms 
	where
		split 			:: [Int] -> [(Int, [Int])]
		split []		= []
		split (x:xs) = foldr (\(y, ys) r -> check (y, x:ys) r) (check (x, xs) []) (split xs)
		perms :: [Int] -> [[Int]]
		perms [] 	= [[]]
		perms xs = foldr (\(v,vs) zs -> foldr (\e r -> checks (v:e) r) zs $ perms vs) [] $ split xs
		check :: (Int, [Int]) -> [(Int, [Int])] -> [(Int, [Int])]
		check (y, zs) r | smooth zs = (y, zs) : r
						| otherwise = r
		checks :: [Int] -> [[Int]] -> [[Int]]
		checks zs r | smooth zs = zs : r
					| otherwise = r
		smooth 				:: [Int] -> Bool
		smooth (w:y:ys) 	= abs (y - w) <= n && smooth (y:ys)
		smooth _ 			= True

		
-- Assignment 8.1

-- | Check with quickCheck the permutations of the smooth_perms_fast function
checkPerms = quickCheck (equalSmoothPerms 4)

-- | Check with quickCheck the length of the smooth_perms_fast function
checkLength = quickCheck (lengthSmoothPerms 4)

-- | Check if the new permutation function gives the same result as the given smooth_perms
equalSmoothPerms n p = all (`elem` (smooth_perms n p)) (smooth_perms_fast n p)

-- | Check if the length of the permutations is the same for every element
lengthSmoothPerms n p = all ((==) (length p) . length) (smooth_perms_fast n p)


-- Assignment 9.1
--
--
-- $exc91
-- Theorem 1 forall xs :: [a], ys :: [a] . length (xs ++ ys) == length (xs) + length (ys)
-- 
-- Proof:
--   Base case, let xs be []
--   
--   @
--     length ([] ++ ys)
--   = Def. ++
--     length (ys)
--   =
--     0 + length (ys)
--   = Def. length
--     length ([]) + length(ys)
--       qed.
--   @
--
--   Inductive case, Hypothesis  length (xs ++ ys) = length (xs) + length (ys)
--
--   @
--     length (x:xs) + length (ys)
--   = Def. length
--     1 + length (xs) + length (ys)
--   = Hypothesis
--     1 + length (xs ++ ys)
--   = Def. length
--     length (x:(xs ++ ys))
--   = Def. ++
--     length ((x:xs) ++ ys)
--       qed.
--   @
-- 
-- 
-- Theorem 2 forall t :: Tree a . length (flatten t) = size t
-- Proof:
--   Base case
--
--   @
--     length (flatten (Leaf x))
--   = Def. flatten
--     length ([x])
--   = Def. []
--     length (x:[])
--   = Def. length
--     1 + length ([])
--   = Def. length
--     1 + 0
--   =
--     1
--   = Def. size
--     size (Leaf x)
--       qed.
--   @
-- 
--   Inductive case, to prove:
--
--   @
--     size (Node l r)
--   = Def. size
--     size l + size r
--   =
--     length ((flatten l) + length (flatten r))
--   = Theorem 1
--     length ((flatten l) ++ (flatten r))
--   = Def. flatten
--     length (flatten (Node l r))
--       qed.
--   @
