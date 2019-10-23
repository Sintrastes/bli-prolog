
module Bli.Prolog.Compiler.Bytecode where

import Data.Serialize
import Data.Bli.Prolog.Ast
import Data.ByteString

toBytecode :: BliProgram -> ByteString
toBytecode = encode

fromBytecode :: ByteString -> Maybe BliProgram
fromBytecode bytecode = 
  case decode @BliProgram bytecode of
    Left _ -> Nothing
    Right program -> Just program