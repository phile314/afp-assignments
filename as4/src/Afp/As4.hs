{-# LANGUAGE TypeOperators, FlexibleInstances #-}
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


class DRead f where
   hreader :: ReadPrec a -> Bool -> ReadPrec (f a)

instance DRead D.U1 where
   hreader _ _ = return D.U1

instance (Read a) => DRead (D.K1 i a) where
   hreader _ _ = liftM D.K1 (readS_to_Prec readsPrec)

instance (DRead f, DRead g) => DRead (f D.:+: g) where
   hreader f r = liftM D.L1 (hreader f r) +++ liftM D.R1 (hreader f r)

instance (DRead f, DRead g) => DRead (f D.:*: g) where
   hreader f r = do l' <- hreader f r
                    when r $ do Punc "," <- lexP
                                return ()
                    r' <- hreader f r
                    return (l' D.:*: r')

instance (D.Constructor c) => DRead (D.M1 D.C c a) where
	hreader f _ = undefined
instance (D.Constructor c) => DRead (D.M1 D.S c a) where
	hreader f _ = undefined

-- dRead :: (D.Generic a, DRead (D.Rep a)) => String -> a
dRead s = case [x |  (x,remain) <- readsPrec 0 s , all isSpace remain] of
           [x] -> x 
           [ ] -> error "no parse"
           _   -> error "ambiguous parse"
