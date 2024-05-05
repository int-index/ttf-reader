module Codec.TTF.Reader.FontFormat
  ( FontFormat(..),
    readFontFormat,
  ) where

import Control.DeepSeq (NFData(..), rwhnf)
import Data.ByteString (ByteString)

import Codec.TTF.Reader.Common

data FontFormat = TrueType | OpenType
  deriving (Eq, Ord, Show)

instance NFData FontFormat where
  rnf = rwhnf

readFontFormat :: ByteString -> Maybe FontFormat
readFontFormat bs =
  case unsafeRead @Word32 0 (expectMinLength 4 bs) of   -- sfntVersion
    0x00010000 -> Just TrueType
    0x74727565 -> Just TrueType
    0x4F54544F -> Just OpenType
    _          -> Nothing
