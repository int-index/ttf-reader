-- Generated with: runhaskell utils/Schema.hs
module Codec.TTF.Reader.Schema.GPOS where

import Codec.TTF.Reader.Common

data GPOSHeader
instance StaticSize GPOSHeader where sizeOf = 10
instance StaticOffset "majorVersion" GPOSHeader Word16 where offsetTo = 0
instance StaticOffset "minorVersion" GPOSHeader Word16 where offsetTo = 2
instance StaticOffset "scriptListOffset" GPOSHeader Word16 where offsetTo = 4
instance StaticOffset "featureListOffset" GPOSHeader Word16 where offsetTo = 6
instance StaticOffset "lookupListOffset" GPOSHeader Word16 where offsetTo = 8

data PairPosTableCommon
instance StaticSize PairPosTableCommon where sizeOf = 8
instance StaticOffset "posFormat" PairPosTableCommon Word16 where offsetTo = 0
instance StaticOffset "coverageOffset" PairPosTableCommon Word16 where offsetTo = 2
instance StaticOffset "valueFormat1" PairPosTableCommon Word16 where offsetTo = 4
instance StaticOffset "valueFormat2" PairPosTableCommon Word16 where offsetTo = 6

data PairPosTableFormat1
instance StaticSize PairPosTableFormat1 where sizeOf = 10
instance StaticOffset "posFormat" PairPosTableFormat1 Word16 where offsetTo = 0
instance StaticOffset "coverageOffset" PairPosTableFormat1 Word16 where offsetTo = 2
instance StaticOffset "valueFormat1" PairPosTableFormat1 Word16 where offsetTo = 4
instance StaticOffset "valueFormat2" PairPosTableFormat1 Word16 where offsetTo = 6
instance StaticOffset "pairSetCount" PairPosTableFormat1 Word16 where offsetTo = 8

data PairSetOffset
instance StaticSize PairSetOffset where sizeOf = 2
instance StaticOffset "pairSetOffset" PairSetOffset Word16 where offsetTo = 0

data PairSet
instance StaticSize PairSet where sizeOf = 2
instance StaticOffset "pairValueCount" PairSet Word16 where offsetTo = 0

data PairValueRecord_4_0
instance StaticSize PairValueRecord_4_0 where sizeOf = 4
instance StaticOffset "secondGlyph" PairValueRecord_4_0 Word16 where offsetTo = 0
instance StaticOffset "xAdvance" PairValueRecord_4_0 Int16 where offsetTo = 2

data PairPosTableFormat2
instance StaticSize PairPosTableFormat2 where sizeOf = 16
instance StaticOffset "posFormat" PairPosTableFormat2 Word16 where offsetTo = 0
instance StaticOffset "coverageOffset" PairPosTableFormat2 Word16 where offsetTo = 2
instance StaticOffset "valueFormat1" PairPosTableFormat2 Word16 where offsetTo = 4
instance StaticOffset "valueFormat2" PairPosTableFormat2 Word16 where offsetTo = 6
instance StaticOffset "classDef1Offset" PairPosTableFormat2 Word16 where offsetTo = 8
instance StaticOffset "classDef2Offset" PairPosTableFormat2 Word16 where offsetTo = 10
instance StaticOffset "class1Count" PairPosTableFormat2 Word16 where offsetTo = 12
instance StaticOffset "class2Count" PairPosTableFormat2 Word16 where offsetTo = 14

data LookupHeader
instance StaticSize LookupHeader where sizeOf = 6
instance StaticOffset "lookupType" LookupHeader Word16 where offsetTo = 0
instance StaticOffset "lookupFlag" LookupHeader Word16 where offsetTo = 2
instance StaticOffset "subTableCount" LookupHeader Word16 where offsetTo = 4

data ClassDefTableCommon
instance StaticSize ClassDefTableCommon where sizeOf = 2
instance StaticOffset "classFormat" ClassDefTableCommon Word16 where offsetTo = 0

data ClassDefTableFormat1
instance StaticSize ClassDefTableFormat1 where sizeOf = 6
instance StaticOffset "classFormat" ClassDefTableFormat1 Word16 where offsetTo = 0
instance StaticOffset "startGlyphID" ClassDefTableFormat1 Word16 where offsetTo = 2
instance StaticOffset "glyphCount" ClassDefTableFormat1 Word16 where offsetTo = 4

data ClassValue
instance StaticSize ClassValue where sizeOf = 2
instance StaticOffset "classValue" ClassValue Word16 where offsetTo = 0

data ClassDefTableFormat2
instance StaticSize ClassDefTableFormat2 where sizeOf = 4
instance StaticOffset "classFormat" ClassDefTableFormat2 Word16 where offsetTo = 0
instance StaticOffset "classRangeCount" ClassDefTableFormat2 Word16 where offsetTo = 2

data ClassRangeRecord
instance StaticSize ClassRangeRecord where sizeOf = 6
instance StaticOffset "startGlyphID" ClassRangeRecord Word16 where offsetTo = 0
instance StaticOffset "endGlyphID" ClassRangeRecord Word16 where offsetTo = 2
instance StaticOffset "class_" ClassRangeRecord Word16 where offsetTo = 4

data CoverageTableCommon
instance StaticSize CoverageTableCommon where sizeOf = 2
instance StaticOffset "coverageFormat" CoverageTableCommon Word16 where offsetTo = 0

data CoverageTableFormat1
instance StaticSize CoverageTableFormat1 where sizeOf = 4
instance StaticOffset "coverageFormat" CoverageTableFormat1 Word16 where offsetTo = 0
instance StaticOffset "glyphCount" CoverageTableFormat1 Word16 where offsetTo = 2

data CoverageGlyphID
instance StaticSize CoverageGlyphID where sizeOf = 2
instance StaticOffset "glyphID" CoverageGlyphID Word16 where offsetTo = 0

data CoverageTableFormat2
instance StaticSize CoverageTableFormat2 where sizeOf = 4
instance StaticOffset "coverageFormat" CoverageTableFormat2 Word16 where offsetTo = 0
instance StaticOffset "rangeCount" CoverageTableFormat2 Word16 where offsetTo = 2

data CoverageRangeRecord
instance StaticSize CoverageRangeRecord where sizeOf = 6
instance StaticOffset "startGlyphID" CoverageRangeRecord Word16 where offsetTo = 0
instance StaticOffset "endGlyphID" CoverageRangeRecord Word16 where offsetTo = 2
instance StaticOffset "startCoverageIndex" CoverageRangeRecord Word16 where offsetTo = 4

data FeatureList
instance StaticSize FeatureList where sizeOf = 2
instance StaticOffset "featureCount" FeatureList Word16 where offsetTo = 0

data FeatureRecord
instance StaticSize FeatureRecord where sizeOf = 6
instance StaticOffset "featureTag" FeatureRecord Word32 where offsetTo = 0
instance StaticOffset "featureOffset" FeatureRecord Word16 where offsetTo = 4

data FeatureTable
instance StaticSize FeatureTable where sizeOf = 4
instance StaticOffset "featureParamsOffset" FeatureTable Word16 where offsetTo = 0
instance StaticOffset "lookupIndexCount" FeatureTable Word16 where offsetTo = 2

data LookupListIndex
instance StaticSize LookupListIndex where sizeOf = 2
instance StaticOffset "lookupListIndex" LookupListIndex Word16 where offsetTo = 0

data LookupList
instance StaticSize LookupList where sizeOf = 2
instance StaticOffset "lookupCount" LookupList Word16 where offsetTo = 0

data LookupOffset
instance StaticSize LookupOffset where sizeOf = 2
instance StaticOffset "lookupOffset" LookupOffset Word16 where offsetTo = 0

data SubtableOffset
instance StaticSize SubtableOffset where sizeOf = 2
instance StaticOffset "subtableOffset" SubtableOffset Word16 where offsetTo = 0