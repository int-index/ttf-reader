-- Generated with: runhaskell utils/Schema.hs
module Codec.TTF.Reader.Schema.Kern where

import Codec.TTF.Reader.Common

data KernHeader
instance StaticSize KernHeader where sizeOf = 4
instance StaticOffset "version" KernHeader Word16 where offsetTo = 0
instance StaticOffset "nTables" KernHeader Word16 where offsetTo = 2

data KernSubtableHeader
instance StaticSize KernSubtableHeader where sizeOf = 6
instance StaticOffset "version" KernSubtableHeader Word16 where offsetTo = 0
instance StaticOffset "length" KernSubtableHeader Word16 where offsetTo = 2
instance StaticOffset "coverage" KernSubtableHeader Word16 where offsetTo = 4

data KernSubtableHeaderFormat0
instance StaticSize KernSubtableHeaderFormat0 where sizeOf = 8
instance StaticOffset "nPairs" KernSubtableHeaderFormat0 Word16 where offsetTo = 0
instance StaticOffset "searchRange" KernSubtableHeaderFormat0 Word16 where offsetTo = 2
instance StaticOffset "entrySelector" KernSubtableHeaderFormat0 Word16 where offsetTo = 4
instance StaticOffset "rangeShift" KernSubtableHeaderFormat0 Word16 where offsetTo = 6

data KernPair
instance StaticSize KernPair where sizeOf = 6
instance StaticOffset "left" KernPair Word16 where offsetTo = 0
instance StaticOffset "right" KernPair Word16 where offsetTo = 2
instance StaticOffset "value" KernPair Int16 where offsetTo = 4