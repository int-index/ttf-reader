module Codec.TTF.Reader.CMap
  ( CMap,
    GlyphId,
    Int(NoGlyphId),
    lookupGlyphId,
    reifyCMap,
    readCMap,
  ) where

import Data.Word
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS.U
import Data.Primitive.PrimArray
import Control.Monad
import Control.Monad.ST
import Control.DeepSeq
import qualified Data.Char as Char

import Codec.TTF.Reader.Common
import qualified Codec.TTF.Reader.Schema.CMap as S

type GlyphId = Int

pattern NoGlyphId :: GlyphId   -- .notdef
pattern NoGlyphId = 0

-- Character to Glyph Index Mapping Table
data CMap =
  MkCMapFormat4
    (PrimArray Int)   -- endCodes        (length = segCount, elements sorted in ascending order)
    (PrimArray Int)   -- startCodes      (length = segCount)
    (PrimArray Int)   -- idDeltas        (length = segCount)
    (PrimArray Int)   -- idRangeOffsets  (length = segCount)
    (PrimArray Int)   -- glyphIdArray    (unknown length)
  deriving Show

instance NFData CMap where
  rnf (MkCMapFormat4 a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

createCMapFormat4 ::
  Int ->      -- segCount
  Int ->      -- glyphIdCount
  ( forall s.
    MutablePrimArray s Int ->
    MutablePrimArray s Int ->
    MutablePrimArray s Int ->
    MutablePrimArray s Int ->
    MutablePrimArray s Int ->
    ST s ()
  ) -> CMap
createCMapFormat4 segCount glyphIdCount cont =
  runST do
    buf_endCodes       <- newPrimArray segCount
    buf_startCodes     <- newPrimArray segCount
    buf_idDeltas       <- newPrimArray segCount
    buf_idRangeOffsets <- newPrimArray segCount
    buf_glyphIdArray   <- newPrimArray glyphIdCount
    cont buf_endCodes buf_startCodes buf_idDeltas buf_idRangeOffsets buf_glyphIdArray
    endCodes       <- unsafeFreezePrimArray buf_endCodes
    startCodes     <- unsafeFreezePrimArray buf_startCodes
    idDeltas       <- unsafeFreezePrimArray buf_idDeltas
    idRangeOffsets <- unsafeFreezePrimArray buf_idRangeOffsets
    glyphIdArray   <- unsafeFreezePrimArray buf_glyphIdArray
    return (MkCMapFormat4 endCodes startCodes idDeltas idRangeOffsets glyphIdArray)

lookupGlyphId :: CMap -> Char -> GlyphId
lookupGlyphId cmap@(MkCMapFormat4 endCodes startCodes _ _ _) c =
  if codepoint < modulus then
    find_segment NoGlyphId with_segment 0 segCount
  else
    NoGlyphId  -- not supported by format 4
  where
    modulus = 65536   -- 2^16

    segCount  = sizeofPrimArray endCodes
    codepoint = Char.ord c

    -- Find the index of the first endCode such that endCode >= codepoint.
    find_segment ::
      r ->           -- default value if the search fails
      (Int -> r)  -> -- continuation if the search succeeds
      Int ->    -- lower bound for the binary search (inclusive)
      Int ->    -- upper bound for the binary search (not inclusive)
      r
    find_segment def cont lower upper =
      if lower < upper then
        let
          i = (lower + upper) `div` 2   -- the middle of the search range
          endCode = indexPrimArray endCodes i
        in
          if endCode >= codepoint then
            -- the endCode is good, but what if there is a smaller one?
            -- continue the search to the left but update the fallback to this endCode
            find_segment (cont i) cont lower i
          else
            -- the endCode is too low, continue the search to the right
            find_segment def cont (i + 1) upper
      else
        def

    with_segment :: Int -> GlyphId
    with_segment i
      | startCode <= codepoint = lookup_glyph_id_in_segment cmap codepoint i
      | otherwise = NoGlyphId
      where
        startCode = indexPrimArray startCodes i

lookup_glyph_id_in_segment :: CMap -> Int -> Int -> GlyphId
lookup_glyph_id_in_segment (MkCMapFormat4 endCodes startCodes idDeltas idRangeOffsets glyphIdArray) codepoint i
  | i == (segCount - 1) && startCode == 0xffff
  = NoGlyphId -- the last segment must map 0xffff to the missing glyph according
              -- to the spec, but not all fonts follow the spec, so we must add
              -- a special case
  | idRangeOffset == 0 = addDelta codepoint
  | otherwise =
      let
        -- See Note [Mad idRangeOffset arithmetic]
        idRangeIndex  = idRangeOffset `div` 2
        skip          = sizeofPrimArray idRangeOffsets - i
        idRangeIndex0 = idRangeIndex - skip
        localIndex    = codepoint - startCode
        glyphIdIndex  = idRangeIndex0 + localIndex
        inBounds      = glyphIdIndex >= 0 && glyphIdIndex < sizeofPrimArray glyphIdArray
        glyphId
          | inBounds  = addDelta (indexPrimArray glyphIdArray glyphIdIndex)
          | otherwise = throw MalformedTTF
      in
        glyphId
  where
    modulus = 65536   -- 2^16

    segCount  = sizeofPrimArray endCodes
    startCode = indexPrimArray startCodes i
    idRangeOffset = indexPrimArray idRangeOffsets i
    idDelta       = indexPrimArray idDeltas       i
    addDelta glyphId = (glyphId + idDelta) `rem` modulus

reifyCMap :: CMap -> [(Char, GlyphId)]
reifyCMap cmap@(MkCMapFormat4 endCodes startCodes _ _ _) =
  let
    segCount = sizeofPrimArray endCodes
    go :: Int -> [(Char, GlyphId)]
    go i = do
      codepoint <- [startCode .. endCode]
      let c = Char.chr codepoint
          glyphId = lookup_glyph_id_in_segment cmap codepoint i
      guard (glyphId /= NoGlyphId)
      [(c, glyphId)]
      where
        startCode = indexPrimArray startCodes i
        endCode   = indexPrimArray endCodes   i
  in
    concatMap go [0 .. segCount - 2]

{- Note [Mad idRangeOffset arithmetic]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
idRangeOffset is a complete clusterfuck of a value, so let's
undo some of the torturous transformations that were applied to
it before saving it into the file.

1. Divide it by 2, because it is a byte offset and we want an array index.
   Our elements (both in idRangeOffsets and glyphIdArray)
   take 2 bytes in the .ttf, so that is our divisor.

   >    idRangeIndex = idRangeOffset `div` 2

2. Now the fun part. idRangeOffset assumes two things:
      a) idRangeOffsets and glyphIdArray are next to each other in memory
      b) we are indexing glyphIdArray from our current position in idRangeOffsets

    Example:

          current                          target
             ↓                               ↓
      [A, B, C, D, E]  [M, N, O, P, Q, R, S, T, U, V]
       idRangeOffsets  glyphIdArray

    Let us say that our current element is C at i=2.
    If we were indexing glyphIdArray from 0, then our target element T would be at j=7.
    But we index from our current position, so there are three remaining elements [C, D, E]
    in idRangeOffsets that separate us from glyphIdArray.

    The number of those elements is included in idRangeOffset
    (actually, their size in bytes, but we already did the
    conversion at the previous step).

    In order to get back to 0-based indexing, subtract!

    >    skip          = sizeofPrimArray idRangeOffsets - i
    >    idRangeIndex0 = idRangeIndex - skip

3. Finally, we are not mapping codepoints to glyph ids.
   We are mapping codepoint /segments/ to /ranges/ of glyph ids.

   Let us say our codepoint is 544 in the range 540..566.
   Then its index within the range is 4 (localIndex = 544 - 540).

   This index is used within the glyph id range, too.
   >    localIndex   = codepoint - startCode
   >    glyphIdIndex = idRangeIndex0 + localIndex
-}

readCMap :: ByteString -> CMap
readCMap bs = readEncodingSubtable subtableOffset bs
  where
    bs_at_cmapHeader = expectRecord @S.CMapHeader bs
    bs_at_encodingRecords = atEnd bs_at_cmapHeader

    numTables      = word16_to_int bs_at_cmapHeader.numTables
    subtableOffset = readSubtableOffset numTables bs_at_encodingRecords

readSubtableOffset :: Int -> ByteString -> Int
readSubtableOffset numTables bs = find_unicode_subtable 0
  where
    -- Linear search to find the first Unicode subtable.
    find_unicode_subtable i | i < numTables =
      if isUnicodeSubtable platformID encodingID then
        subtableOffset
      else
        find_unicode_subtable (i+1)
      where
        bs_at_encodingRecord = unsafeDropTo @S.EncodingRecord i bs_at_encodingRecords
        platformID     = bs_at_encodingRecord.platformID
        encodingID     = bs_at_encodingRecord.encodingID
        subtableOffset = word32_to_int bs_at_encodingRecord.subtableOffset
    find_unicode_subtable _ = throw UnsupportedTTF

    bs_at_encodingRecords = expectMinLength (sizeOfArr @S.EncodingRecord numTables) bs

isUnicodeSubtable :: Word16 -> Word16 -> Bool
isUnicodeSubtable platformID encodingID =
  (platformID == 0 && encodingID >= 0 && encodingID <= 4) ||
  (platformID == 3 && encodingID == 1)

readEncodingSubtable :: Int -> ByteString -> CMap
readEncodingSubtable subtableOffset bs =
  case bs_at_header.format of
    4 -> readEncodingSubtableFormat4 (untagBytes bs_at_header)
    _ -> throw UnsupportedTTF
  where
    bs_at_header = expectRecordAtOffset @S.CMapSubtableHeader subtableOffset bs

readEncodingSubtableFormat4 :: ByteString -> CMap
readEncodingSubtableFormat4 bs =
  createCMapFormat4 segCount glyphIdCount
    \buf_endCodes buf_startCodes buf_idDeltas buf_idRangeOffsets buf_glyphIdArray -> do
      forIndices segCount \i -> do
        let endCode       = word16_to_int (unsafeReadAtIndex 0 i bs_at_endCodes)
            startCode     = word16_to_int (unsafeReadAtIndex 0 i bs_at_startCodes)
            idDelta       = int16_to_int  (unsafeReadAtIndex 0 i bs_at_idDeltas)
            idRangeOffset = word16_to_int (unsafeReadAtIndex 0 i bs_at_idRangeOffsets)
        writePrimArray buf_endCodes       i endCode
        writePrimArray buf_startCodes     i startCode
        writePrimArray buf_idDeltas       i idDelta
        writePrimArray buf_idRangeOffsets i idRangeOffset
      forIndices glyphIdCount \i -> do
        let glyphId = word16_to_int (unsafeReadAtIndex 0 i bs_at_glyphIdArray)
        writePrimArray buf_glyphIdArray i glyphId
  where
    format4_segments_size = 8 * segCount + 2

    at_endCodes       = 0
    at_startCodes     = 2 * segCount + 2
    at_idDeltas       = 4 * segCount + 2
    at_idRangeOffsets = 6 * segCount + 2
    at_glyphIdArray   = format4_segments_size

    bs_at_header = expectRecord @S.CMapSubtableFormat4Header bs
    bs_at_segments =
      expectMinLength format4_segments_size $
      atEnd bs_at_header

    segCount = word16_to_int bs_at_header.segCountX2 `div` 2

    !bs_at_endCodes       = BS.U.unsafeDrop at_endCodes       bs_at_segments
    !bs_at_startCodes     = BS.U.unsafeDrop at_startCodes     bs_at_segments
    !bs_at_idDeltas       = BS.U.unsafeDrop at_idDeltas       bs_at_segments
    !bs_at_idRangeOffsets = BS.U.unsafeDrop at_idRangeOffsets bs_at_segments
    !bs_at_glyphIdArray   = BS.U.unsafeDrop at_glyphIdArray   bs_at_segments

    -- The rest of the ByteString stores the glyphIdArray
    glyphIdCount = BS.length bs_at_glyphIdArray `div` 2