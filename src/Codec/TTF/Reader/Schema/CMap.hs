-- Generated with: runhaskell utils/Schema.hs
module Codec.TTF.Reader.Schema.CMap where

import Codec.TTF.Reader.Common

data CMapHeader
instance StaticSize CMapHeader where sizeOf = 4
instance StaticOffset "version" CMapHeader Word16 where offsetTo = 0
instance StaticOffset "numTables" CMapHeader Word16 where offsetTo = 2

data CMapSubtableHeader
instance StaticSize CMapSubtableHeader where sizeOf = 2
instance StaticOffset "format" CMapSubtableHeader Word16 where offsetTo = 0

data CMapSubtableFormat4Header
instance StaticSize CMapSubtableFormat4Header where sizeOf = 14
instance StaticOffset "format" CMapSubtableFormat4Header Word16 where offsetTo = 0
instance StaticOffset "length" CMapSubtableFormat4Header Word16 where offsetTo = 2
instance StaticOffset "language" CMapSubtableFormat4Header Word16 where offsetTo = 4
instance StaticOffset "segCountX2" CMapSubtableFormat4Header Word16 where offsetTo = 6
instance StaticOffset "searchRange" CMapSubtableFormat4Header Word16 where offsetTo = 8
instance StaticOffset "entrySelector" CMapSubtableFormat4Header Word16 where offsetTo = 10
instance StaticOffset "rangeShift" CMapSubtableFormat4Header Word16 where offsetTo = 12

data EncodingRecord
instance StaticSize EncodingRecord where sizeOf = 8
instance StaticOffset "platformID" EncodingRecord Word16 where offsetTo = 0
instance StaticOffset "encodingID" EncodingRecord Word16 where offsetTo = 2
instance StaticOffset "subtableOffset" EncodingRecord Word32 where offsetTo = 4