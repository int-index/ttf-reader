module Codec.TTF.Reader.TableDirectory
  ( TableTag(..),
    TableRecords,
    lookupTableRecord,
    getTableData,
    TableRecord(NoTableRecord, ..),
    readTableDirectory,
  ) where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS.U
import Data.Primitive.PrimArray
import Control.Monad.ST
import Control.DeepSeq
import Data.List (sortBy)
import Data.Functor.Classes
import Data.Foldable (for_)
import GHC.IsList

import Codec.TTF.Reader.Common
import qualified Codec.TTF.Reader.Schema.TableDirectory as S


{- HLINT ignore "Use camelCase" -}
data TableTag =
    TableTag_cmap
  | TableTag_head
  | TableTag_hhea
  | TableTag_hmtx
  | TableTag_maxp
  | TableTag_name
  | TableTag_OS_2   -- "OS/2"
  | TableTag_post
  | TableTag_cvt
  | TableTag_fpgm
  | TableTag_glyf
  | TableTag_loca
  | TableTag_prep
  | TableTag_gasp
  | TableTag_CFF
  | TableTag_CFF2
  | TableTag_VORG
  | TableTag_SVG
  | TableTag_EBDT
  | TableTag_EBLC
  | TableTag_EBSC
  | TableTag_CBDT
  | TableTag_CBLC
  | TableTag_sbix
  | TableTag_BASE
  | TableTag_GDEF
  | TableTag_GPOS
  | TableTag_GSUB
  | TableTag_JSTF
  | TableTag_MATH
  | TableTag_avar
  | TableTag_cvar
  | TableTag_fvar
  | TableTag_gvar
  | TableTag_HVAR
  | TableTag_MVAR
  | TableTag_STAT
  | TableTag_VVAR
  | TableTag_COLR
  | TableTag_CPAL
  | TableTag_DSIG
  | TableTag_hdmx
  | TableTag_kern
  | TableTag_LTSH
  | TableTag_MERG
  | TableTag_meta
  | TableTag_PCLT
  | TableTag_VDMX
  | TableTag_vhea
  | TableTag_vmtx
  deriving (Eq, Ord, Enum, Bounded, Show)

instance NFData TableTag where
  rnf = rwhnf

tableTagCount :: Int
tableTagCount = fromEnum (maxBound :: TableTag) + 1

matchTableTag :: Word32 -> Maybe TableTag
matchTableTag tag =
  -- Generated with: runhaskell utils/TagLits.hs
  case tag of
    0x636d6170 -> Just TableTag_cmap
    0x68656164 -> Just TableTag_head
    0x68686561 -> Just TableTag_hhea
    0x686d7478 -> Just TableTag_hmtx
    0x6d617870 -> Just TableTag_maxp
    0x6e616d65 -> Just TableTag_name
    0x4f532f32 -> Just TableTag_OS_2
    0x706f7374 -> Just TableTag_post
    0x63767420 -> Just TableTag_cvt
    0x6670676d -> Just TableTag_fpgm
    0x676c7966 -> Just TableTag_glyf
    0x6c6f6361 -> Just TableTag_loca
    0x70726570 -> Just TableTag_prep
    0x67617370 -> Just TableTag_gasp
    0x43464620 -> Just TableTag_CFF
    0x43464632 -> Just TableTag_CFF2
    0x564f5247 -> Just TableTag_VORG
    0x53564720 -> Just TableTag_SVG
    0x45424454 -> Just TableTag_EBDT
    0x45424c43 -> Just TableTag_EBLC
    0x45425343 -> Just TableTag_EBSC
    0x43424454 -> Just TableTag_CBDT
    0x43424c43 -> Just TableTag_CBLC
    0x73626978 -> Just TableTag_sbix
    0x42415345 -> Just TableTag_BASE
    0x47444546 -> Just TableTag_GDEF
    0x47504f53 -> Just TableTag_GPOS
    0x47535542 -> Just TableTag_GSUB
    0x4a535446 -> Just TableTag_JSTF
    0x4d415448 -> Just TableTag_MATH
    0x61766172 -> Just TableTag_avar
    0x63766172 -> Just TableTag_cvar
    0x66766172 -> Just TableTag_fvar
    0x67766172 -> Just TableTag_gvar
    0x48564152 -> Just TableTag_HVAR
    0x4d564152 -> Just TableTag_MVAR
    0x53544154 -> Just TableTag_STAT
    0x56564152 -> Just TableTag_VVAR
    0x434f4c52 -> Just TableTag_COLR
    0x4350414c -> Just TableTag_CPAL
    0x44534947 -> Just TableTag_DSIG
    0x68646d78 -> Just TableTag_hdmx
    0x6b65726e -> Just TableTag_kern
    0x4c545348 -> Just TableTag_LTSH
    0x4d455247 -> Just TableTag_MERG
    0x6d657461 -> Just TableTag_meta
    0x50434c54 -> Just TableTag_PCLT
    0x56444d58 -> Just TableTag_VDMX
    0x76686561 -> Just TableTag_vhea
    0x766d7478 -> Just TableTag_vmtx
    _          -> Nothing   -- See Note [Unrecognized table tags]

{- Note [Unrecognized table tags]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some fonts contain non-standard table tags that we do not support.
For example, FontForge can generate "TeX" and "FFTM" tables.
Rather than abort parsing, we choose to ignore unrecognized tables.
-}

data TableRecord =
  MkTableRecord {
    offset :: Int,
    length :: Int    -- Invariant: (offset == 0) implies (length == 0)
  }
  deriving (Eq, Show)

-- The offset is never 0 in an actual table record, so we can abuse this field
-- to indicate a missing table
pattern NoTableRecord :: TableRecord
pattern NoTableRecord <- ((.offset) -> 0)
  where NoTableRecord = noTableRecord

noTableRecord :: TableRecord
noTableRecord = MkTableRecord 0 0

data TableRecords =
  MkTableRecords
    (PrimArray Int)   -- offset components
      -- exactly tableTagCount elements,
      -- the 0 offset indicates a missing table
    (PrimArray Int)   -- length components
      -- exactly tableTagCount elements,
      -- 0 if the corresponding offset is 0
  deriving Eq

instance NFData TableRecords where
  rnf (MkTableRecords a b) = rnf a `seq` rnf b

lookupTableRecord :: TableRecords -> TableTag -> TableRecord
lookupTableRecord tableRecords tag =
  MkTableRecord {
    offset = indexPrimArray offset_components i,
    length = indexPrimArray length_components i
  }
  where
    (MkTableRecords offset_components length_components) = tableRecords
    i = fromEnum tag

-- Sorted by offset
tableRecordsToList :: TableRecords -> [(TableTag, Int, Int)]
tableRecordsToList tableRecords =
  sortBy (\(_, offset1, _) (_, offset2, _) -> compare offset1 offset2) $
    [
      (tag, tableRecord.offset, tableRecord.length)
        | tag <- [minBound..maxBound],
          let tableRecord = lookupTableRecord tableRecords tag,
          tableRecord.offset /= 0
    ]

createTableRecords :: (forall s. MutablePrimArray s Int -> MutablePrimArray s Int -> ST s ()) -> TableRecords
createTableRecords cont =
  runST do
    buf_offset_components <- newPrimArray tableTagCount
    buf_length_components <- newPrimArray tableTagCount
    setPrimArray buf_offset_components 0 tableTagCount 0
    setPrimArray buf_length_components 0 tableTagCount 0
    cont buf_offset_components buf_length_components
    offset_components <- unsafeFreezePrimArray buf_offset_components
    length_components <- unsafeFreezePrimArray buf_length_components
    return (MkTableRecords offset_components length_components)

tableRecordsFromList :: [(TableTag, Int, Int)] -> TableRecords
tableRecordsFromList triples =
  createTableRecords \buf_offset_component buf_length_component -> do
    for_ triples \(tableTag, tableRecord_offset, tableRecord_length) -> do
      let buf_pos = fromEnum tableTag
      writePrimArray buf_offset_component buf_pos tableRecord_offset
      writePrimArray buf_length_component buf_pos tableRecord_length

instance IsList TableRecords where
  type Item TableRecords = (TableTag, Int, Int)
  toList = tableRecordsToList
  fromList = tableRecordsFromList

instance Show TableRecords where
  showsPrec =
    showsUnaryWith
      (\p tableRecords -> showsPrec p (toList tableRecords))
      "fromList"

readTableDirectory :: ByteString -> TableRecords
readTableDirectory bs =
  createTableRecords \buf_offset_components buf_length_components ->
    forIndices numTables \i -> do
      let bs_at_tableRecord = unsafeDropTo @S.TableRecord i bs_at_tableRecords
      case matchTableTag bs_at_tableRecord.tableTag of
        Nothing ->
          -- Note [Unrecognized table tags]
          return ()
        Just tableTag -> do
          let tableRecord_offset = word32_to_int bs_at_tableRecord.offset
              tableRecord_length = word32_to_int bs_at_tableRecord.length
              buf_pos = fromEnum tableTag
          writePrimArray buf_offset_components buf_pos tableRecord_offset
          writePrimArray buf_length_components buf_pos tableRecord_length
  where
    bs_at_tableDirectoryHeader = expectRecord @S.TableDirectoryHeader bs
    numTables = word16_to_int bs_at_tableDirectoryHeader.numTables
    bs_at_tableRecords =
      expectMinLength (sizeOfArr @S.TableRecord numTables) $
      atEnd bs_at_tableDirectoryHeader

-- Returns a slice (without copying) of the original ByteString,
-- but with offsets adjusted to the specified TTF table.
--
-- Caution: do not forget to check for NoTableRecord if the table is optional.
getTableData :: ByteString -> TableRecord -> ByteString
getTableData bs tableRecord =
  BS.U.unsafeTake tableRecord.length $
  BS.U.unsafeDrop tableRecord.offset $
  expectMinLength (tableRecord.offset + tableRecord.length) bs