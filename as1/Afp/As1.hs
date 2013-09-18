module Afp.As1
  (test, count)
where

-- Assignment 1.1
-- Cabalize everything
-- what should we put in the package name?


-- Assignment 2.5

-- Creating a varargs function in Haskell is explained on http://www.haskell.org/haskellwiki/Varargs
-- There is another, different version on http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn ,
-- but it uses language extensions.

test :: [Int]
test = [count, count 1 2 3, count "" [True, False] id (+)]
test' :: [Int]
test' = [count', count' 1 2 3, count' "" [True, False] id (+)]

class Countable b where
  counti :: Int -> b
  counti' :: Int -> b

instance Countable Int where
  counti _ = 0
  counti' = id

instance Countable b => Countable (a -> b) where
  counti _ = \x -> counti 0
  counti' acc = \x -> counti' (acc + 1)

-- count always returns zero.
count :: Countable a => a
count = counti 0

-- count' returns the number of arguments given.
count' :: Countable a => a
count' = counti' 0


-- Assignment 7.1

split []     = []
split (x:xs) = (x,xs):[(y,x:ys) | (y,ys) <- split xs]

perms [] = [[]]
perms xs = [(v:p) | (v,vs) <- split xs,p <- perms vs]

smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _        = True

smooth_perms :: Int -> [Int] -> [[Int]]
smooth_perms n xs = filter (smooth n) (perms xs)
-- Assignment 8.1



-- Assignment 9.1
--
--
