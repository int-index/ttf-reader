module Codec.TTF.Reader.GPOS
  ( GPOS,
    lookupGPOSKerning,
    readGPOS,

    GPOS_Lookup(..),
    GPOS_LookupList(..),
    gposLookupListToOffsets,
    GPOS_FeatureRecord(..),
    GPOS_PairPosCommon(..),
    CoverageTable(..),
    PairAdjustmentTable(..),
    PairSet(..),
    pairSetToList,
    ClassDef(..),
    reifyClassDef,
    GPOS_DebugView(..),
    readGPOS_DebugView,
    gposFromDebugView,
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Primitive.PrimArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS.U
import Data.Primitive.Array
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as Map

import Codec.TTF.Reader.Common
import Codec.TTF.Reader.CMap (GlyphId)
import qualified Codec.TTF.Reader.Schema.GPOS as S

data GPOS =
  MkGPOS {
    coverageTables :: Array CoverageTable,
    pairAdjustmentTables :: Array PairAdjustmentTable   -- 1-to-1 correspondence with coverageTables
  } deriving (Eq, Show)

instance NFData GPOS where
  rnf (MkGPOS a b) = rnf a `seq` rnf b

type CoverageIndex = Int

data CoverageTable =
  MkCoverageTableFormat1 {
    glyphArray :: PrimArray GlyphId -- ordered
  } |
  MkCoverageTableFormat2 {
    startGlyphIds :: PrimArray GlyphId, -- ordered
    endGlyphIds :: PrimArray GlyphId,
    startCoverageIndices :: PrimArray CoverageIndex
  } deriving (Eq, Show)


instance NFData CoverageTable where
  rnf (MkCoverageTableFormat1 a) = rnf a
  rnf (MkCoverageTableFormat2 a b c) = rnf a `seq` rnf b `seq` rnf c

data PairAdjustmentTable =
  MkPairAdjustmentTableFormat1 {
    pairSets :: Array PairSet -- ordered, the same index as in coverage table
  }
  |
  MkPairAdjustmentTableFormat2 {
    classDef1 :: ClassDef,
    classDef2 :: ClassDef,
    classToKernMap :: Array (PrimArray XAdvance)
  }
  deriving (Eq, Show)

instance NFData PairAdjustmentTable where
  rnf (MkPairAdjustmentTableFormat1 p) = rnf p
  rnf (MkPairAdjustmentTableFormat2 a b c) = rnf a `seq` rnf b `seq` rnf c

data PairSet =
  MkPairSet {
    secondGlyphs :: PrimArray GlyphId,
    xAdvances :: PrimArray XAdvance
  } deriving (Eq, Show)

pairSetToList :: PairSet -> [(GlyphId, XAdvance)]
pairSetToList ps = do
  i <- [ 0 .. sizeofPrimArray ps.secondGlyphs - 1]
  pure (indexPrimArray ps.secondGlyphs i, indexPrimArray ps.xAdvances i)

instance NFData PairSet where
  rnf (MkPairSet a b) = rnf a `seq` rnf b

type XAdvance = Int

data ClassDef =
  ClassDefFormat1 {
    startGlyphID :: GlyphId,
    classValueArray :: PrimArray ClassId
  } |
  ClassDefFormat2 {
    startGlyphIds :: PrimArray GlyphId, -- ordered
    endGlyphIds :: PrimArray GlyphId,
    classes :: PrimArray ClassId
  } deriving (Eq, Show)

type ClassId = Int

instance NFData ClassDef where
  rnf (ClassDefFormat1 a b) = rnf a `seq` rnf b
  rnf (ClassDefFormat2 a b c) = rnf a `seq` rnf b `seq` rnf c

binary_search :: PrimArray GlyphId -> GlyphId -> Maybe Int
binary_search arr ref = go Nothing 0 (sizeofPrimArray arr) where
  go ::
    Maybe Int ->
    Int ->            -- lower bound for the binary search (inclusive)
    Int ->            -- upper bound for the binary search (not inclusive)
    Maybe Int
  go def lower upper =
    if lower < upper then
        case compare ref val of
          LT -> go (Just i) lower i
          EQ -> Just i
          GT -> go def (i+1) upper
      else
        def
      where
        i = (lower + upper) `div` 2   -- the middle of the search range
        val = indexPrimArray arr i

rangeBinarySearch :: PrimArray GlyphId -> PrimArray GlyphId -> GlyphId -> Maybe Int
rangeBinarySearch lower upper ref = case binary_search upper ref of
  Just i | let a = indexPrimArray lower i
         , LT /= compare ref a -> Just i
  _ -> Nothing

strictBinarySearch :: PrimArray GlyphId -> GlyphId -> Maybe Int
strictBinarySearch arr ref = case binary_search arr ref of
  Just i | let a = indexPrimArray arr i
         , EQ == compare ref a -> Just i
  _ -> Nothing

-- ClassDef: all glyphs not explicitly assigned a class fall into Class 0
reifyClassDef :: ClassDef -> [(ClassId, [GlyphId])]
reifyClassDef ClassDefFormat1{startGlyphID, classValueArray} =
  Map.toList $ Map.fromListWith (++) do
    i <- [0 .. sizeofPrimArray classValueArray]
    let glyph = startGlyphID + i
        cls   = indexPrimArray classValueArray i
    [(cls, [glyph])]
reifyClassDef ClassDefFormat2{startGlyphIds, endGlyphIds, classes} =
  Map.toList $ Map.fromListWith (++) do
    i <- [0 .. sizeofPrimArray startGlyphIds]
    let startGl = indexPrimArray startGlyphIds i
        endGl   = indexPrimArray endGlyphIds i
        cls     = indexPrimArray classes i
    glyph <- [startGl .. endGl]
    [(cls, [glyph])]

-- Format1: Any glyph not included in the range of covered glyph IDs automatically belongs to Class 0.
-- Format2: Any glyph not covered by a ClassRangeRecord is assumed to belong to Class 0.
lookupGlyphClass :: ClassDef -> GlyphId -> ClassId
lookupGlyphClass classDef glyph =
  case classDef of
    ClassDefFormat1{startGlyphID,classValueArray} ->
      let i = glyph - startGlyphID in
      if (i >= 0 && i < sizeofPrimArray classValueArray)
        then (indexPrimArray classValueArray i)
        else 0
    ClassDefFormat2{startGlyphIds, endGlyphIds, classes} ->
      case rangeBinarySearch startGlyphIds endGlyphIds glyph of
        Just i  -> indexPrimArray classes i
        Nothing -> 0

lookupCoverageIndex :: CoverageTable -> GlyphId -> Maybe CoverageIndex
lookupCoverageIndex MkCoverageTableFormat1{glyphArray} glyph1 = strictBinarySearch glyphArray glyph1
lookupCoverageIndex MkCoverageTableFormat2{startGlyphIds, endGlyphIds, startCoverageIndices} glyph1 =
    fmap getCoverageIndex (rangeBinarySearch startGlyphIds endGlyphIds glyph1)
  where
    getCoverageIndex i =
      let startGl = indexPrimArray startGlyphIds i
          startCoverageIndex = indexPrimArray startCoverageIndices i
      in startCoverageIndex + (glyph1 - startGl)

lookupGPOSKerning :: GPOS -> GlyphId -> GlyphId -> Maybe XAdvance
lookupGPOSKerning gpos glyph1 glyph2 = go 0
  where
    n = sizeofArray gpos.coverageTables
    go i | i < n = m_xAdvance <|> go (i + 1)
      where
        coverageTable = indexArray gpos.coverageTables i
        paTable       = indexArray gpos.pairAdjustmentTables i
        m_xAdvance    = do
          coverageIndex <- lookupCoverageIndex coverageTable glyph1
          lookupXAdvance paTable coverageIndex
    go _ = Nothing

    lookupXAdvance MkPairAdjustmentTableFormat1{pairSets} coverageIndex
      | coverageIndex >= 0 && coverageIndex < sizeofArray pairSets =
        let MkPairSet{secondGlyphs, xAdvances} = indexArray pairSets coverageIndex
            m_glyph2Index = strictBinarySearch secondGlyphs glyph2
        in fmap (indexPrimArray xAdvances) m_glyph2Index
      | otherwise = throw MalformedTTF
    lookupXAdvance MkPairAdjustmentTableFormat2{classDef1, classDef2, classToKernMap} _ = do
      let class1 = lookupGlyphClass classDef1 glyph1
          class2 = lookupGlyphClass classDef2 glyph2
      guard (class1 >= 0 && class1 < sizeofArray classToKernMap)
      let class2ToKernMap = indexArray classToKernMap class1
      guard (class2 >= 0 && class2 < sizeofPrimArray class2ToKernMap)
      pure (indexPrimArray class2ToKernMap class2)

readGPOS :: ByteString -> GPOS
readGPOS bs_at_gpos = checkVersion bs_at_gposHeader gpos
  where
    bs_at_gposHeader  = expectRecord @S.GPOSHeader bs_at_gpos
    featureListOffset = word16_to_int bs_at_gposHeader.featureListOffset
    lookupListOffset  = word16_to_int bs_at_gposHeader.lookupListOffset
    -- scriptListOffset = ...
    -- We decided that we don't care about scripts tables, because
    -- almost all the fonts from the test data just fill all the features
    -- into array of featureIndices for each language system in the font

    bs_at_featureList = expectRecordAtOffset @S.FeatureList featureListOffset bs_at_gpos
    bs_at_lookupList  = expectRecordAtOffset @S.LookupList lookupListOffset bs_at_gpos
    kernIndices = kernIndicesInFeatureList bs_at_featureList
    lookupList  = getLookupList bs_at_lookupList
    kernLookupTables = map (getLookupTable lookupList) kernIndices
    pairPosCommon = concatMap readPairPosCommon kernLookupTables
    coverageTables = map readCoverageTable pairPosCommon
    pairAdjustmentTables = map readPairAdjustmentTable pairPosCommon
    gpos = collectGPOS coverageTables pairAdjustmentTables

checkVersion :: TaggedBytes S.GPOSHeader -> r -> r
checkVersion bs_at_gposHeader r =
  case (# majorVersion, minorVersion #) of
    (# 1, 0 #) -> r
    _ -> throw UnsupportedTTF
  where
    majorVersion = bs_at_gposHeader.majorVersion
    minorVersion = bs_at_gposHeader.minorVersion

data GPOS_Lookup =
  MkGPOS_Lookup {
    offset :: Int,   -- from bs_at_lookupList
    bs_at_lookupHeader :: TaggedBytes S.LookupHeader,
    lookupType :: Word16,
    lookupFlag :: Word16,
    subtableOffsets :: ~[Int]
  }

data GPOS_LookupList =
  MkGPOS_LookupList {
    bs_at_lookupList :: TaggedBytes S.LookupList,
    lookupCount :: Int,
    lookupFn :: Int -> Int
  }

gposLookupListToOffsets :: GPOS_LookupList -> [Int]
gposLookupListToOffsets a = map a.lookupFn [0 .. a.lookupCount - 1]

getLookupList :: TaggedBytes S.LookupList -> GPOS_LookupList
getLookupList bs_at_lookupList =
  let
    lookupCount = word16_to_int bs_at_lookupList.lookupCount
    lookupFn lookupListIndex
      | lookupListIndex >= 0 && lookupListIndex < lookupCount =
          word16_to_int (unsafeDropTo @S.LookupOffset lookupListIndex bs_at_lookupOffsets).lookupOffset
      | otherwise = throw MalformedTTF
    bs_at_lookupOffsets =
      expectMinLength (sizeOfArr @S.LookupOffset lookupCount) $
      atEnd bs_at_lookupList
  in
    MkGPOS_LookupList {bs_at_lookupList, lookupCount, lookupFn}

getLookupTable :: GPOS_LookupList -> Int -> GPOS_Lookup
getLookupTable lookupList lookupListIndex =
  let
    offset = lookupList.lookupFn lookupListIndex
    bs_at_lookupHeader = expectRecordAtOffset @S.LookupHeader offset (untagBytes lookupList.bs_at_lookupList)
    lookupType    = bs_at_lookupHeader.lookupType
    lookupFlag    = bs_at_lookupHeader.lookupFlag
    subTableCount = word16_to_int bs_at_lookupHeader.subTableCount
    bs_at_subTableOffsets =
      expectMinLength (sizeOfArr @S.SubtableOffset subTableCount) $
      atEnd bs_at_lookupHeader
    getSubtableOffset i = word16_to_int (unsafeDropTo @S.SubtableOffset i bs_at_subTableOffsets).subtableOffset
    subtableOffsets = map getSubtableOffset [0 .. subTableCount - 1]
  in
    MkGPOS_Lookup{offset, bs_at_lookupHeader, lookupType, lookupFlag, subtableOffsets}

isPairAdjustmentTable :: GPOS_Lookup -> Bool
isPairAdjustmentTable lookupTable = lookupTable.lookupType == 2

data GPOS_PairPosCommon =
  MkGPOS_PairPosCommon {
    subtableOffset :: Int,   -- from bs_at_lookupHeader
    bs_at_pairPosTableCommon :: TaggedBytes S.PairPosTableCommon,
    posFormat :: Word16,
    coverageOffset :: Int    -- from bs_at_pairPosTableCommon
  }

checkPairPosValueFormat :: TaggedBytes S.PairPosTableCommon -> r -> r
checkPairPosValueFormat bs_at_pairPosTableCommon r =
  case (# valueFormat1, valueFormat2 #) of
    (# 4, 0 #) -> r
    _ -> throw UnsupportedTTF
  where
    valueFormat1 = bs_at_pairPosTableCommon.valueFormat1
    valueFormat2 = bs_at_pairPosTableCommon.valueFormat2

readPairPosCommon :: GPOS_Lookup -> [GPOS_PairPosCommon]
readPairPosCommon lookupTable = do
  guard (isPairAdjustmentTable lookupTable)
  subtableOffset <- lookupTable.subtableOffsets
  let bs_at_pairPosTableCommon =
        expectRecordAtOffset @S.PairPosTableCommon subtableOffset
          (untagBytes lookupTable.bs_at_lookupHeader)
      posFormat      = bs_at_pairPosTableCommon.posFormat
      coverageOffset = word16_to_int bs_at_pairPosTableCommon.coverageOffset
  return $
    checkPairPosValueFormat bs_at_pairPosTableCommon $
    MkGPOS_PairPosCommon {
      subtableOffset,
      bs_at_pairPosTableCommon,
      posFormat,
      coverageOffset
    }

readPairAdjustmentTable :: GPOS_PairPosCommon -> PairAdjustmentTable
readPairAdjustmentTable pairPosCommon =
  case pairPosCommon.posFormat of
    1 -> parsePairPosTableFormat1 (untagBytes pairPosCommon.bs_at_pairPosTableCommon)
    2 -> parsePairPosTableFormat2 (untagBytes pairPosCommon.bs_at_pairPosTableCommon)
    _ -> throw MalformedTTF

parsePairPosTableFormat1 :: ByteString -> PairAdjustmentTable
parsePairPosTableFormat1 bs =
  createPairAdjustmentTableFormat1 pairSetCount \buf_pairSets ->
    forIndices pairSetCount \i -> do
      let
        bs_at_pairSetOffset = unsafeDropTo @S.PairSetOffset i bs_at_pairSetOffsets
        pairSetOffset = word16_to_int bs_at_pairSetOffset.pairSetOffset
        bs_at_pairSet = expectRecordAtOffset @S.PairSet pairSetOffset bs
        pairValueCount = word16_to_int bs_at_pairSet.pairValueCount
        !bs_at_pairValueRecords =
          expectMinLength (sizeOfArr @S.PairValueRecord_4_0 pairValueCount) $
          atEnd bs_at_pairSet
        pairSet = createPairSet pairValueCount \buf_secondGlyphs buf_xAdvances ->
          forIndices pairValueCount \j -> do
            let
              bs_at_pairValueRecord = unsafeDropTo @S.PairValueRecord_4_0 j bs_at_pairValueRecords
              secondGlyph = word16_to_int bs_at_pairValueRecord.secondGlyph
              xAdvance = int16_to_int bs_at_pairValueRecord.xAdvance
            writePrimArray buf_secondGlyphs j secondGlyph
            writePrimArray buf_xAdvances j xAdvance
      writeArray buf_pairSets i $! pairSet
  where
    bs_at_pairPosTableFormat1 = expectRecord @S.PairPosTableFormat1 bs
    pairSetCount         = word16_to_int bs_at_pairPosTableFormat1.pairSetCount
    bs_at_pairSetOffsets =
      expectMinLength (sizeOfArr @S.PairSetOffset pairSetCount) $
      atEnd bs_at_pairPosTableFormat1

createPairAdjustmentTableFormat1 ::
  Int ->
  (forall s. MutableArray s PairSet -> ST s ()) -> PairAdjustmentTable
createPairAdjustmentTableFormat1 size cont =
  runST do
    buf_pairSets <- newArray size (error "createPairAdjustmentTableFormat1: impossible happened!")
    cont buf_pairSets
    pairSets <- unsafeFreezeArray buf_pairSets
    pure MkPairAdjustmentTableFormat1{pairSets}

createPairSet :: Int -> (forall s . MutablePrimArray s GlyphId -> MutablePrimArray s XAdvance -> ST s ()) -> PairSet
createPairSet size cont =
  runST do
    buf_secondGlyphs <- newPrimArray size
    buf_xAdvances <- newPrimArray size
    cont buf_secondGlyphs buf_xAdvances
    secondGlyphs <- unsafeFreezePrimArray buf_secondGlyphs
    xAdvances <- unsafeFreezePrimArray buf_xAdvances
    pure MkPairSet{secondGlyphs, xAdvances}

parsePairPosTableFormat2 :: ByteString -> PairAdjustmentTable
parsePairPosTableFormat2 bs =
  if BS.length bs_at_class1Records < class1Records_size then
    throw MalformedTTF
  else
  let
    classToKernMap = createClassToKernMap class1Count class2Count \i j ->
      let
        bs_at_class1Record = BS.U.unsafeDrop (i * class1Record_size) bs_at_class1Records
        bs_at_class2Record = BS.U.unsafeDrop (j * class2Record_size) bs_at_class1Record
      in int16_to_int (unsafeRead @Int16 0 bs_at_class2Record)
  in
    MkPairAdjustmentTableFormat2{classDef1, classDef2, classToKernMap}
  where
    bs_at_pairPosTableFormat2 = expectRecord @S.PairPosTableFormat2 bs

    class1Records_size = class1Count * class1Record_size
    class1Record_size  = class2Count * class2Record_size

    class2Record_size = 2  -- int16
      -- valueFormat1 = 0x0004 X_ADVANCE, valueRecord1 = {int16 xAdvance}
      -- valueFormat2 = 0x0000,           valueRecord2 = {}
      -- checked in checkPairPosValueFormat

    bs_at_class1Records   = atEnd bs_at_pairPosTableFormat2

    classDef1Offset = word16_to_int bs_at_pairPosTableFormat2.classDef1Offset
    classDef2Offset = word16_to_int bs_at_pairPosTableFormat2.classDef2Offset
    class1Count     = word16_to_int bs_at_pairPosTableFormat2.class1Count
    class2Count     = word16_to_int bs_at_pairPosTableFormat2.class2Count

    classDef1 = readClassDef (expectRecordAtOffset @S.ClassDefTableCommon classDef1Offset bs)
    classDef2 = readClassDef (expectRecordAtOffset @S.ClassDefTableCommon classDef2Offset bs)

createClassToKernMap :: Int -> Int -> (Int -> Int -> XAdvance) -> Array (PrimArray XAdvance)
createClassToKernMap x y cont =
  runST do
    buf_class1Records <- newArray x mempty
    forIndices x \i -> do
      buf_class2Records <- newPrimArray y
      forIndices y \j ->
        writePrimArray buf_class2Records j (cont i j)
      class2Records <- unsafeFreezePrimArray buf_class2Records
      writeArray buf_class1Records i class2Records
    unsafeFreezeArray buf_class1Records

readClassDef :: TaggedBytes S.ClassDefTableCommon -> ClassDef
readClassDef bs_at_classDefTableCommon  =
  case bs_at_classDefTableCommon.classFormat of
    1 -> readClassTableFormat1 bs_at_classDefTableCommon
    2 -> readClassTableFormat2 bs_at_classDefTableCommon
    _ -> throw MalformedTTF

readClassTableFormat1 :: TaggedBytes S.ClassDefTableCommon -> ClassDef
readClassTableFormat1 bs_at_classDefTableCommon = ClassDefFormat1{startGlyphID, classValueArray}
  where
    bs_at_classDefTableFormat1 =
      expectRecord @S.ClassDefTableFormat1 (untagBytes bs_at_classDefTableCommon)
    startGlyphID = word16_to_int bs_at_classDefTableFormat1.startGlyphID
    glyphCount = word16_to_int bs_at_classDefTableFormat1.glyphCount
    bs_at_classValueArray =
      expectMinLength (sizeOfArr @S.ClassValue glyphCount) $
      atEnd bs_at_classDefTableFormat1
    classValueArray = generatePrimArray glyphCount \i ->
      let bs_at_classValue = unsafeDropTo @S.ClassValue i bs_at_classValueArray
      in word16_to_int bs_at_classValue.classValue

readClassTableFormat2 :: TaggedBytes S.ClassDefTableCommon -> ClassDef
readClassTableFormat2 bs_at_classDefTableCommon =
  createClassDefFormat2 classRangeCount \buf_startGlyphIds buf_endGlyphIds buf_classes ->
    forIndices classRangeCount \i -> do
      let
        bs_at_classRangeRecord = unsafeDropTo @S.ClassRangeRecord i bs_at_classRangeRecords
        startGlyphID = word16_to_int bs_at_classRangeRecord.startGlyphID
        endGlyphID = word16_to_int bs_at_classRangeRecord.endGlyphID
        class_ = word16_to_int bs_at_classRangeRecord.class_
      writePrimArray buf_startGlyphIds i startGlyphID
      writePrimArray buf_endGlyphIds i endGlyphID
      writePrimArray buf_classes i class_
  where
    bs_at_classDefTableFormat2 =
      expectRecord @S.ClassDefTableFormat2 (untagBytes bs_at_classDefTableCommon)
    classRangeCount = word16_to_int bs_at_classDefTableFormat2.classRangeCount
    bs_at_classRangeRecords =
      expectMinLength (sizeOfArr @S.ClassRangeRecord classRangeCount) $
      atEnd bs_at_classDefTableFormat2

createClassDefFormat2 ::
  Int ->
  ( forall s.
    MutablePrimArray s GlyphId ->
    MutablePrimArray s GlyphId ->
    MutablePrimArray s ClassId ->
    ST s ()
  ) -> ClassDef
createClassDefFormat2 size cont =
  runST do
    buf_startGlyphIds <- newPrimArray size
    buf_endGlyphIds <- newPrimArray size
    buf_classes <- newPrimArray size

    cont buf_startGlyphIds buf_endGlyphIds buf_classes

    startGlyphIds <- unsafeFreezePrimArray buf_startGlyphIds
    endGlyphIds <- unsafeFreezePrimArray buf_endGlyphIds
    classes <- unsafeFreezePrimArray buf_classes

    pure ClassDefFormat2{startGlyphIds, endGlyphIds, classes}

readCoverageTable :: GPOS_PairPosCommon -> CoverageTable
readCoverageTable pairPosCommon =
  case bs_at_coverageTableCommon.coverageFormat of
    1 -> readCoverageTableFormat1 bs_at_coverageTableCommon
    2 -> readCoverageTableFormat2 bs_at_coverageTableCommon
    _ -> throw MalformedTTF
  where
    bs_at_coverageTableCommon =
      expectRecordAtOffset @S.CoverageTableCommon
        pairPosCommon.coverageOffset
        (untagBytes pairPosCommon.bs_at_pairPosTableCommon)

readCoverageTableFormat1 :: TaggedBytes S.CoverageTableCommon -> CoverageTable
readCoverageTableFormat1 bs_at_coverageTableCommon =
    MkCoverageTableFormat1{glyphArray}
  where
    glyphCount = word16_to_int bs_at_coverageTableFormat1.glyphCount
    bs_at_glyphArray =
      expectMinLength (sizeOfArr @S.CoverageGlyphID glyphCount) $
      atEnd bs_at_coverageTableFormat1
    glyphArray = generatePrimArray glyphCount \i ->
      let bs_at_glyph = unsafeDropTo @S.CoverageGlyphID i bs_at_glyphArray
      in word16_to_int bs_at_glyph.glyphID
    bs_at_coverageTableFormat1 =
      expectRecord @S.CoverageTableFormat1 (untagBytes bs_at_coverageTableCommon)

readCoverageTableFormat2 :: TaggedBytes S.CoverageTableCommon -> CoverageTable
readCoverageTableFormat2 bs_at_coverageTableCommon =
  createCoverageTableFormat2 rangeCount
    \ buf_startGlyphIds
      buf_endGlyphIds
      buf_startCoverageIndices ->
    forIndices rangeCount \i -> do
      let
        bs_at_rangeRecord = unsafeDropTo @S.CoverageRangeRecord i bs_at_rangeRecords
        startGlyphId = word16_to_int bs_at_rangeRecord.startGlyphID
        endGlyphId = word16_to_int bs_at_rangeRecord.endGlyphID
        startCoverageIndex = word16_to_int bs_at_rangeRecord.startCoverageIndex
      writePrimArray buf_startGlyphIds i startGlyphId
      writePrimArray buf_endGlyphIds i endGlyphId
      writePrimArray buf_startCoverageIndices i startCoverageIndex
  where
    rangeCount = word16_to_int bs_at_coverageTableFormat2.rangeCount
    bs_at_rangeRecords =
      expectMinLength (sizeOfArr @S.CoverageRangeRecord rangeCount) $
      atEnd bs_at_coverageTableFormat2
    bs_at_coverageTableFormat2 =
      expectRecord @S.CoverageTableFormat2 (untagBytes bs_at_coverageTableCommon)

createCoverageTableFormat2 ::
  Int ->
  ( forall s.
    MutablePrimArray s GlyphId ->
    MutablePrimArray s GlyphId ->
    MutablePrimArray s CoverageIndex ->
    ST s ()
  ) -> CoverageTable
createCoverageTableFormat2 rangeCount cont =
  runST do
    buf_startGlyphIds <- newPrimArray rangeCount
    buf_endGlyphIds <- newPrimArray rangeCount
    buf_startCoverageIndices <- newPrimArray rangeCount

    cont buf_startGlyphIds buf_endGlyphIds buf_startCoverageIndices

    startGlyphIds <- unsafeFreezePrimArray buf_startGlyphIds
    endGlyphIds <- unsafeFreezePrimArray buf_endGlyphIds
    startCoverageIndices <- unsafeFreezePrimArray buf_startCoverageIndices

    pure MkCoverageTableFormat2{startGlyphIds, endGlyphIds, startCoverageIndices}

collectGPOS :: [CoverageTable] -> [PairAdjustmentTable] -> GPOS
collectGPOS coverageTables pairAdjustmentTables =
  MkGPOS {
    coverageTables       = arrayFromList coverageTables,
    pairAdjustmentTables = arrayFromList pairAdjustmentTables
  }

data GPOS_FeatureRecord =
  MkGPOS_FeatureRecord {
    featureTag :: Word32,
    featureOffset :: Int
  } deriving (Eq, Ord)

getFeatureRecords :: TaggedBytes S.FeatureList -> [GPOS_FeatureRecord]
getFeatureRecords bs_at_featureList =
  nubOrd do -- Sometimes we get duplicates, e.g. in STIXTwoText-Regular.ttf
    i <- [0 .. featureCount - 1]
    let bs_at_featureRecord = unsafeDropTo @S.FeatureRecord i bs_at_featureRecords
        featureTag    = bs_at_featureRecord.featureTag
        featureOffset = word16_to_int bs_at_featureRecord.featureOffset
    [MkGPOS_FeatureRecord{featureTag, featureOffset}]
  where
    featureCount = word16_to_int bs_at_featureList.featureCount
    bs_at_featureRecords =
      expectMinLength (sizeOfArr @S.FeatureRecord featureCount) $
      atEnd bs_at_featureList

filterKernFeatureRecords :: [GPOS_FeatureRecord] -> [GPOS_FeatureRecord]
filterKernFeatureRecords = filter (\r -> r.featureTag == kernTag)
  where
    kernTag = 0x6b65726e -- "kern"

featureLookupListIndices :: TaggedBytes S.FeatureTable -> [Int]
featureLookupListIndices bs_at_featureTable =
    map readLookupListIndex [0 .. lookupIndexCount - 1]
  where
    lookupIndexCount = word16_to_int bs_at_featureTable.lookupIndexCount
    bs_at_lookupListIndices =
      expectMinLength (sizeOfArr @S.LookupListIndex lookupIndexCount) $
      atEnd bs_at_featureTable
    readLookupListIndex i =
      word16_to_int (unsafeDropTo @S.LookupListIndex i bs_at_lookupListIndices).lookupListIndex

checkFeatureParamsOffset :: TaggedBytes S.FeatureTable -> r -> r
checkFeatureParamsOffset bs_at_featureTable r
  | bs_at_featureTable.featureParamsOffset == 0 = r
  | otherwise = throw UnsupportedTTF

kernIndicesInFeatureList :: TaggedBytes S.FeatureList -> [Int]
kernIndicesInFeatureList bs_at_featureList =
  do
    MkGPOS_FeatureRecord{featureOffset} <- filterKernFeatureRecords (getFeatureRecords bs_at_featureList)
    let bs_at_featureTable = expectRecordAtOffset @S.FeatureTable featureOffset (untagBytes bs_at_featureList)
    checkFeatureParamsOffset bs_at_featureTable $
      featureLookupListIndices bs_at_featureTable

data GPOS_DebugView =
  MkGPOS_DebugView {
    featureRecords :: [GPOS_FeatureRecord],
    kernIndices :: [Int],
    lookupList :: GPOS_LookupList,
    kernLookupTables :: [GPOS_Lookup],
    pairPosCommon :: [GPOS_PairPosCommon],
    coverageTables :: [CoverageTable],
    pairAdjustmentTables :: [PairAdjustmentTable]
  }

readGPOS_DebugView :: ByteString -> GPOS_DebugView
readGPOS_DebugView bs_at_gpos =
  checkVersion bs_at_gposHeader $
    MkGPOS_DebugView {
      featureRecords,
      kernIndices,
      lookupList,
      kernLookupTables,
      pairPosCommon,
      coverageTables,
      pairAdjustmentTables
    }
  where
    bs_at_gposHeader  = expectRecord @S.GPOSHeader bs_at_gpos
    featureListOffset = word16_to_int bs_at_gposHeader.featureListOffset
    lookupListOffset  = word16_to_int bs_at_gposHeader.lookupListOffset
    bs_at_featureList = expectRecordAtOffset @S.FeatureList featureListOffset bs_at_gpos
    featureRecords    = getFeatureRecords bs_at_featureList
    kernIndices       = kernIndicesInFeatureList bs_at_featureList
    bs_at_lookupList  = expectRecordAtOffset @S.LookupList lookupListOffset bs_at_gpos
    lookupList        = getLookupList bs_at_lookupList
    kernLookupTables  = map (getLookupTable lookupList) kernIndices
    pairPosCommon     = concatMap readPairPosCommon kernLookupTables
    coverageTables    = map readCoverageTable pairPosCommon
    pairAdjustmentTables = map readPairAdjustmentTable pairPosCommon

gposFromDebugView :: GPOS_DebugView -> GPOS
gposFromDebugView d = collectGPOS d.coverageTables d.pairAdjustmentTables