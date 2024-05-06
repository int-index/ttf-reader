module Codec.TTF.Reader.Head (
  readUnitsPerEm
) where


import Data.ByteString (ByteString)
import Codec.TTF.Reader.Common
import qualified Codec.TTF.Reader.Schema.Head as S

readUnitsPerEm :: ByteString -> Int
readUnitsPerEm bs = unitsPerEm where
  bs_at_head = expectRecord @S.Head bs
  unitsPerEm = word16_to_int bs_at_head.unitsPerEm
