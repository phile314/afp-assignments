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

class Count b where
  counti :: Int -> b
  counti' :: Int -> b

instance Count Int where
  counti _ = 0
  counti' = id

instance Count b => Count (a -> b) where
  counti _ = \x -> counti 0
  counti' acc = \x -> counti' (acc + 1)

-- count always returns zero.
count :: Count a => a
count = counti 0

-- count' returns the number of arguments given.
count' :: Count a => a
count' = counti' 0


-- Assignment 7.1



-- Assignment 8.1



-- Assignment 9.1
