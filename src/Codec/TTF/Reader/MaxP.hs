module Codec.TTF.Reader.MaxP
  ( readNumGlyphs
  ) where

import Control.Exception
import Data.ByteString (ByteString)

import Codec.TTF.Reader.Common
import qualified Codec.TTF.Reader.Schema.MaxP as S

readNumGlyphs :: ByteString -> Int
readNumGlyphs bs =
  case version of
    0x00005000 -> numGlyphs
    0x00010000 -> numGlyphs
    _          -> throw MalformedTTF
  where
    bs_at_maxpHeader = expectRecord @S.MaxPHeader bs
    version   = bs_at_maxpHeader.version
    numGlyphs = word16_to_int bs_at_maxpHeader.numGlyphs
