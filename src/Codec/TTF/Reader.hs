-- | Read information from a TTF file.
module Codec.TTF.Reader
  ( TTFException(..),
    module Codec.TTF.Reader.FontFormat,
    module Codec.TTF.Reader.TableDirectory,
    module Codec.TTF.Reader.CMap,
    module Codec.TTF.Reader.MaxP,
    module Codec.TTF.Reader.Kern,
    module Codec.TTF.Reader.GPOS,
  ) where

import Codec.TTF.Reader.Common
import Codec.TTF.Reader.FontFormat
import Codec.TTF.Reader.TableDirectory
import Codec.TTF.Reader.CMap
import Codec.TTF.Reader.MaxP
import Codec.TTF.Reader.Kern
import Codec.TTF.Reader.GPOS
