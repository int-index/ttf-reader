-- Generated with: runhaskell utils/Schema.hs
module Codec.TTF.Reader.Schema.TableDirectory where

import Codec.TTF.Reader.Common

data TableDirectoryHeader
instance StaticSize TableDirectoryHeader where sizeOf = 12
instance StaticOffset "sfntVersion" TableDirectoryHeader Word32 where offsetTo = 0
instance StaticOffset "numTables" TableDirectoryHeader Word16 where offsetTo = 4
instance StaticOffset "searchRange" TableDirectoryHeader Word16 where offsetTo = 6
instance StaticOffset "entrySelector" TableDirectoryHeader Word16 where offsetTo = 8
instance StaticOffset "rangeShift" TableDirectoryHeader Word16 where offsetTo = 10

data TableRecord
instance StaticSize TableRecord where sizeOf = 16
instance StaticOffset "tableTag" TableRecord Word32 where offsetTo = 0
instance StaticOffset "checksum" TableRecord Word32 where offsetTo = 4
instance StaticOffset "offset" TableRecord Word32 where offsetTo = 8
instance StaticOffset "length" TableRecord Word32 where offsetTo = 12