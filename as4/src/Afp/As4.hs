module Afp.As4
  ( -- * Task 10.3

    -- * Task 11.1

    -- * Task 11.2
  )
where

import Data.Generics

-- 10.3
-- ph
--
-- TODO



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



-- 11.2
