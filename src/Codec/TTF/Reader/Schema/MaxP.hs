-- Generated with: runhaskell utils/Schema.hs
module Codec.TTF.Reader.Schema.MaxP where

import Codec.TTF.Reader.Common

data MaxPHeader
instance StaticSize MaxPHeader where sizeOf = 6
instance StaticOffset "version" MaxPHeader Word32 where offsetTo = 0
instance StaticOffset "numGlyphs" MaxPHeader Word16 where offsetTo = 4