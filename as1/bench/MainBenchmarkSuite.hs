module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Afp.As1


main :: IO ()
main = newStdGen >>= (\g -> defaultMain $ benchmarks g)

smooth_perms_to_test = [("smooth_perms", smooth_perms, (<10)), ("smooth_perms_fast", smooth_perms_fast, (<12))]

benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen = concat $ map mkBench smooth_perms_to_test
  where dist = 10
        mkBench (lbl, f, c) = [bench (lbl ++ " - size " ++ (show i)) $ nf (f dist) (randList i) | i <- [2,4,6,8, 10, 12,14,16], c i]
        randList i = let min = -dist * i
                         max = dist * i in take i $ randomRs (min, max) gen

