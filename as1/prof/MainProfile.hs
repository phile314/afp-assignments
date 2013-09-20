module Main (main) where

import Afp.As1
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  f <- return $ choose $ head args
  let r = f 20 [7, 4, -12, -34, 4, 25, 32, -18, 23, 12, 0, 31]
  let m = sum (map sum r)
  print $ "Number of perms " ++ (show $ length r) ++ ", Sum is " ++ (show m)

choose "--simple" = smooth_perms
choose "--fast"   = smooth_perms_fast
choose "--tree"   = smooth_perms_tree
