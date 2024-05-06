-- Generated with: runhaskell utils/Schema.hs
module Codec.TTF.Reader.Schema.Head where

import Codec.TTF.Reader.Common

data Head
instance StaticSize Head where sizeOf = 54
instance StaticOffset "majorVersion" Head Word16 where offsetTo = 0
instance StaticOffset "minorVersion" Head Word16 where offsetTo = 2
-- | fixed-point number (16.16)
instance StaticOffset "fontRevision" Head Int32 where offsetTo = 4
instance StaticOffset "checksumAdjustment" Head Word32 where offsetTo = 8
instance StaticOffset "magicNumber" Head Word32 where offsetTo = 12
instance StaticOffset "flags" Head Word16 where offsetTo = 16
instance StaticOffset "unitsPerEm" Head Word16 where offsetTo = 18
-- | Date and time represented in number of seconds since 12:00 midnight, January 1, 1904, UTC.
instance StaticOffset "created" Head Int64 where offsetTo = 20
-- | Date and time represented in number of seconds since 12:00 midnight, January 1, 1904, UTC.
instance StaticOffset "modified" Head Int64 where offsetTo = 28
instance StaticOffset "xMin" Head Int16 where offsetTo = 36
instance StaticOffset "yMin" Head Int16 where offsetTo = 38
instance StaticOffset "xMax" Head Int16 where offsetTo = 40
instance StaticOffset "yMax" Head Int16 where offsetTo = 42
instance StaticOffset "macStyle" Head Word16 where offsetTo = 44
instance StaticOffset "lowestRecPPEM" Head Word16 where offsetTo = 46
instance StaticOffset "fontDirectionHint" Head Int16 where offsetTo = 48
instance StaticOffset "indexToLocFormat" Head Int16 where offsetTo = 50
instance StaticOffset "glyphDataFormat" Head Int16 where offsetTo = 52