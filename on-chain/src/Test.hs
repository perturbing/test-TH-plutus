module Test where

import           Prelude                     (IO, error, id, either, (.), Integer, (*))

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as BS16

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = either error id . BS16.decode

multiplyInt :: Integer -> Integer
multiplyInt x = x*x