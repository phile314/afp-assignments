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


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen = [bench "simple_solution" $ nf (smooth_perms 5) (intGen 42)]

intGen :: Int -> [Int]
intGen seed = unGen arbitrary (mkStdGen seed) 10
