module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.QuickCheck2
 
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
  [
    testGroup "Signal shifts"
    [
--     testProperty "L/R one shift composition" propCyclicOneShiftIdentity1
--     , testProperty "Left shift identity" propLeftShiftIdentity
    ]
  ]
