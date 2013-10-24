module Afp.As4.Messages where


type NickName = String

data MsgC2S
  = Join NickName
  | Speak String
  | Leave
  deriving (Show, Read)

data MsgS2C
  = Joined NickName
  | Left NickName
  | Spoke NickName String
  deriving (Show, Read)
