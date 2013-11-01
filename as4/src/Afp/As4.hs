{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures #-}
module Afp.As4
  ( -- * Task 11.1

    -- * Task 11.2
  )
where

import Data.Generics
import qualified Generics.Deriving as D
import Control.Monad
import Text.Read
import Data.Char
import Test.QuickCheck


-- 11.1
--
--

-- | A show function for a datatype using the syb library
gshowx :: (Data a) => a -> String
gshowx = (\x -> "(" ++ showConstr (toConstr x) ++
		concat (gmapQ ((++) " " . gshowx) x) ++ ")"
	) `extQ` (show :: String -> String)


-- | A read function for a datatype using the syb library
greadx :: Data a => String -> a
greadx s = either error id (readEither' s)
	where readEither' s = case [x | (x, _) <- gread s] of 
		(x:_)	-> Right x
		_		-> Left "read: failed to parse"

-- | Test for the property of show and read: read . show == id
prop_ShowRead :: (Eq a, Data a) => a -> Bool
prop_ShowRead x = x == (greadx $ gshowx x) 


-- 11.2

dReadsPrec :: DReads a => Int -> ReadS a
dReadsPrec n = readPrec_to_S dRead n

dReads :: (DReads a) => String -> a
dReads s = case [x |  (x,remain) <- dReadsPrec 0 s , all isSpace remain] of
           [x] -> x 
           [ ] -> error "no parse"
           _   -> error "ambiguous parse"

class DReads a where
	dRead :: ReadPrec a
	default dRead :: (D.Generic a, DRead (D.Rep a)) => ReadPrec a
	dRead = liftM  D.to (dReader undefined False)

class DRead f where
   dReader :: ReadPrec a -> Bool -> ReadPrec (f a)

instance DRead D.U1 where
   dReader _ _ = return D.U1

instance (DReads a) => DRead (D.K1 i a) where
   dReader _ _ = liftM D.K1 dRead

instance (DRead f, DRead g) => DRead (f D.:+: g) where
   dReader f r = liftM D.L1 (dReader f r) +++ liftM D.R1 (dReader f r)

instance (DRead f, DRead g) => DRead (f D.:*: g) where
   dReader f r = do l' <- dReader f r
                    when r $ do Punc "," <- lexP
                                return ()
                    r' <- dReader f r
                    return (l' D.:*: r')

instance (D.Constructor c) => DRead (D.M1 D.C c a) where
	dReader f _ = undefined
instance (D.Constructor c) => DRead (D.M1 D.S c a) where
	dReader f _ = undefined
instance (D.Constructor c) => DRead (D.M1 D.D c a) where
	dReader f _ = undefined
