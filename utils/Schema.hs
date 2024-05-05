{-# LANGUAGE PatternSynonyms #-}

module Schema where

import Data.List

tableDirectorySchema :: Schema Int
tableDirectorySchema =
  mkSchema "TableDirectory" [
    "TableDirectoryHeader" := [
      "sfntVersion"   :# uint32,
      "numTables"     :# uint16,
      "searchRange"   :# uint16,
      "entrySelector" :# uint16,
      "rangeShift"    :# uint16
    ],
    "TableRecord" := [
      "tableTag" :# uint32,
      "checksum" :# uint32,
      "offset"   :# uint32,
      "length"   :# uint32
    ]
  ]

cmapSchema :: Schema Int
cmapSchema =
  mkSchema "CMap" [
    "CMapHeader" := [
      "version"   :# uint16,
      "numTables" :# uint16
    ],
    "CMapSubtableHeader" := [
      "format" :# uint16
    ],
    "CMapSubtableFormat4Header" := [
      "format"        :# uint16,
      "length"        :# uint16,
      "language"      :# uint16,
      "segCountX2"    :# uint16,
      "searchRange"   :# uint16,
      "entrySelector" :# uint16,
      "rangeShift"    :# uint16
    ],
    "EncodingRecord" := [
      "platformID"     :# uint16,
      "encodingID"     :# uint16,
      "subtableOffset" :# uint32
    ]
  ]

maxpSchema :: Schema Int
maxpSchema =
  mkSchema "MaxP" [
    "MaxPHeader" := [
      "version"   :# uint32,
      "numGlyphs" :# uint16
    ]
  ]

kernSchema :: Schema Int
kernSchema =
  mkSchema "Kern" [
    "KernHeader" := [
      "version" :# uint16,
      "nTables" :# uint16
    ],
    "KernSubtableHeader" := [
      "version"  :# uint16,
      "length"   :# uint16,
      "coverage" :# uint16
    ],
    "KernSubtableHeaderFormat0" := [
      "nPairs"        :# uint16,
      "searchRange"   :# uint16,
      "entrySelector" :# uint16,
      "rangeShift"    :# uint16
    ],
    "KernPair" := [
      "left"  :# uint16,
      "right" :# uint16,
      "value" :# int16
    ]
  ]

gposSchema :: Schema Int
gposSchema =
  mkSchema "GPOS" [
    "GPOSHeader" := [
      "majorVersion" :# uint16,
      "minorVersion" :# uint16,
      "scriptListOffset"  :# uint16,
      "featureListOffset" :# uint16,
      "lookupListOffset"  :# uint16
    ],
    "PairPosTableCommon" := [
      -- The common prefix of PairPosTableFormat1 and PairPosTableFormat2 subtables
      "posFormat"       :# uint16,
      "coverageOffset"  :# uint16,
      "valueFormat1"    :# uint16,
      "valueFormat2"    :# uint16
    ],
    "PairPosTableFormat1" := [
      "posFormat"       :# uint16,
      "coverageOffset"  :# uint16,
      "valueFormat1"    :# uint16,
      "valueFormat2"    :# uint16,
      "pairSetCount"    :# uint16
      -- pairSetOffsets :: PairSetOffset * pairSetCount
    ],
    "PairSetOffset" := [
      "pairSetOffset" :# uint16
    ],
    "PairSet" := [
      "pairValueCount" :# uint16
      -- pairValueRecords :: PairValueRecord * pairValueCount
    ],
    "PairValueRecord_4_0" := [
      -- valueFormat1 = 4, valueFormat2 = 0
      "secondGlyph" :# uint16,
      "xAdvance"    :# int16
    ],
    "PairPosTableFormat2" := [
      "posFormat"       :# uint16,
      "coverageOffset"  :# uint16,
      "valueFormat1"    :# uint16,
      "valueFormat2"    :# uint16,
      "classDef1Offset" :# uint16,
      "classDef2Offset" :# uint16,
      "class1Count"     :# uint16,
      "class2Count"     :# uint16
    ],
    "LookupHeader" := [
      "lookupType" :# uint16,
      "lookupFlag" :# uint16,
      "subTableCount" :# uint16
    ],
    "ClassDefTableCommon" := [
      "classFormat" :# uint16
    ],
    "ClassDefTableFormat1" := [
      "classFormat"  :# uint16,
      "startGlyphID" :# uint16,
      "glyphCount"   :# uint16
      -- classValueArray :: ClassValue * glyphCount
    ],
    "ClassValue" := [
      "classValue" :# uint16
    ],
    "ClassDefTableFormat2" := [
      "classFormat"     :# uint16,
      "classRangeCount" :# uint16
      -- classRangeRecords :: ClassRangeRecord * classRangeCount
    ],
    "ClassRangeRecord" := [
      "startGlyphID" :# uint16,
      "endGlyphID"   :# uint16,
      "class_"       :# uint16
    ],
    "CoverageTableCommon" := [
      "coverageFormat" :# uint16
    ],
    "CoverageTableFormat1" := [
      "coverageFormat" :# uint16,
      "glyphCount"     :# uint16
      -- glyphArray :: GlyphID * glyphCount
    ],
    "CoverageGlyphID" := [
      "glyphID" :# uint16
    ],
    "CoverageTableFormat2" := [
      "coverageFormat" :# uint16,
      "rangeCount"     :# uint16
      -- rangeRecords :: RangeRecord * rangeCount
    ],
    "CoverageRangeRecord" := [
      "startGlyphID"       :# uint16,
      "endGlyphID"         :# uint16,
      "startCoverageIndex" :# uint16
    ],
    "FeatureList" := [
      "featureCount" :# uint16
      -- featureRecords :: FeatureRecord * featureCount
    ],
    "FeatureRecord" := [
      "featureTag"    :# uint32,
      "featureOffset" :# uint16
    ],
    "FeatureTable" := [
      "featureParamsOffset" :# uint16,
      "lookupIndexCount"    :# uint16
      -- lookupListIndices :: LookupListIndex * lookupIndexCount
    ],
    "LookupListIndex" := [
      "lookupListIndex" :# uint16
    ],
    "LookupList" := [
      "lookupCount" :# uint16
      -- lookupOffsets :: LookupOffset * lookupCount
    ],
    "LookupOffset" := [
      "lookupOffset" :# uint16
    ],
    "SubtableOffset" := [
      "subtableOffset" :# uint16
    ]
  ]

--------------------------------------------------------

data TyInfo = TyInfo { tyName :: String, tySize :: Int }
  deriving Show

uint16, uint32, int16 :: TyInfo
uint16 = TyInfo "Word16" 2
uint32 = TyInfo "Word32" 4
int16  = TyInfo "Int16"  2

data FieldInfo a = MkFieldInfo { fieldName :: String, fieldTy :: TyInfo, fieldOffset :: a }

pattern (:#) :: String -> TyInfo -> FieldInfo ()
pattern a :# b = MkFieldInfo a b ()

data RecInfo a = MkRecInfo { recName :: String, recFields :: [FieldInfo a], recSize :: a }

pattern (:=) :: String -> [FieldInfo ()] -> RecInfo ()
pattern a := b = MkRecInfo a b ()

data Schema a = MkSchema String [RecInfo a]

mkSchema :: String -> [RecInfo ()] -> Schema Int
mkSchema name recInfos = MkSchema name (map computeRecInfoOffsets recInfos)

computeRecInfoOffsets :: RecInfo () -> RecInfo Int
computeRecInfoOffsets (recName := flds) = MkRecInfo{recName, recFields, recSize}
  where
    (recSize, recFields) = mapAccumL computeFieldInfoOffset 0 flds
    computeFieldInfoOffset offset (fieldName :# fieldTy) = (offset', fld')
      where
        offset' = offset + tySize fieldTy
        fld' = MkFieldInfo{fieldName, fieldTy, fieldOffset = offset}

pprSchema :: Schema Int -> ShowS
pprSchema (MkSchema name recInfos) s =
  "-- Generated with: runhaskell utils/Schema.hs\n" ++
  "module Codec.TTF.Reader.Schema." ++ name ++ " where\n\n" ++
  "import Codec.TTF.Reader.Common" ++
  foldr pprRecInfo s recInfos

pprRecInfo :: RecInfo Int -> ShowS
pprRecInfo MkRecInfo{recName, recFields, recSize} s =
  "\n\ndata " ++ recName ++
  "\ninstance StaticSize " ++ recName ++ " where sizeOf = " ++ show recSize ++
  foldr (pprFieldInfo recName) s recFields

pprFieldInfo :: String -> FieldInfo Int -> ShowS
pprFieldInfo recName MkFieldInfo{fieldName, fieldTy, fieldOffset} s =
  "\ninstance StaticOffset " ++ show fieldName
    ++ " " ++ recName
    ++ " " ++ tyName fieldTy
    ++ " where offsetTo = "
    ++ show fieldOffset
    ++ s

writeSchema :: Schema Int -> IO ()
writeSchema schema@(MkSchema name _) =
  writeFile
    ("src/Codec/TTF/Reader/Schema/" ++ name ++ ".hs")
    (pprSchema schema "")

main :: IO ()
main = do
  writeSchema tableDirectorySchema
  writeSchema cmapSchema
  writeSchema maxpSchema
  writeSchema kernSchema
  writeSchema gposSchema