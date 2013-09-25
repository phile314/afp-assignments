module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Afp.As1 
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
  [
    testGroup "Smooth perms"
    [
     testProperty "" allSmoothPerms,
     testProperty "" lengthSmoothPerms
    ]
  ]
