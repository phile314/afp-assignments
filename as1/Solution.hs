module Main where

-- Creating a varargs function in Haskell is explained on http://www.haskell.org/haskellwiki/Varargs
-- There is another, different version on http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn ,
-- but it uses language extensions.

test :: [Int]
test = [count, count 1 2 3, count "" [True, False] id (+)]

class Count b where
  count' :: Int -> b
  count'' :: Int -> b

instance Count Int where
  count' _ = 0
  count'' = id

instance Count b => Count (a -> b) where
  count' _ = \x -> count' 0
  count'' acc = \x -> count'' (acc + 1)

-- count' always returns zero,
-- count'' returns the number of arguments given
count :: Count a => a
count = count'' 0
