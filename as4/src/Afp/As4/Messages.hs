module Afp.As4.Messages where

-- import for Text.Read copied functions
import Text.ParserCombinators.ReadP as P
-- end Text.Read imports
import Text.Read

type NickName = String

data MsgC2S
  = CJoin NickName
  | CSpeak String
  | CInvalidMsg
  | CEOF
  deriving (Show, Read)

data MsgS2C
  = SJoined NickName
  | SLeft NickName
  | SSpoke NickName String
  | SInvalidMsg
  | SEOF
  deriving (Show, Read)

class Msg a where
    errMsg :: a
    eofMsg :: a
    toStr :: a -> String
    fromStr :: String -> Maybe a

instance Msg MsgC2S where
    errMsg = CInvalidMsg
    eofMsg = CEOF
    toStr = show
    fromStr = readMaybe

instance Msg MsgS2C where
    errMsg = SInvalidMsg
    eofMsg = SEOF
    toStr = show
    fromStr = readMaybe

-------------------------------------
---- The following code is copied from base-4.6.0.1, as it is not in base-4.5, but we don't want to depend on the newer version....

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
