module Codec.TTF.Reader.Kern
  ( Kern,
    lookupKern,
    readKern,
  ) where

import Data.Word
import Data.Bits
import Control.Exception
import Data.ByteString (ByteString)
import Data.Primitive.PrimArray
import Control.Monad.ST
import Control.DeepSeq
import Data.Functor.Classes
import Data.Foldable (for_)
import GHC.IsList

import Codec.TTF.Reader.Common
import Codec.TTF.Reader.CMap (GlyphId)
import qualified Codec.TTF.Reader.Schema.Kern as S

-- | The Legacy Kerning Table
data Kern =
  MkKernFormat0
    (PrimArray Int)   -- leftrightPairs  (length = nPairs, elements sorted in ascending order)
    (PrimArray Int)   -- values          (length = nPairs)
  deriving Eq

kernToList :: Kern -> [(GlyphId, GlyphId, Int)]
kernToList (MkKernFormat0 leftrightPairs values) =
  [
    (left, right, value)
      | i <- [0 .. nPairs - 1],
        let (left, right) = splitLeftRightPair (indexPrimArray leftrightPairs i)
            value = indexPrimArray values  i
  ]
  where
    nPairs = sizeofPrimArray leftrightPairs

kernFromList :: [(GlyphId, GlyphId, Int)] -> Kern
kernFromList triples =
  createKernFormat0 nPairs \buf_leftrightPairs buf_values ->
    for_ (zip [0..] triples) \(i, (left, right, value)) -> do
      let leftrightPair = mkLeftRightPair left right
      writePrimArray buf_leftrightPairs i leftrightPair
      writePrimArray buf_values i value
  where
    nPairs = length triples

instance NFData Kern where
  rnf (MkKernFormat0 a b) = rnf a `seq` rnf b

instance IsList Kern where
  type Item Kern = (GlyphId, GlyphId, Int)
  toList = kernToList
  fromList = kernFromList

instance Show Kern where
  showsPrec =
    showsUnaryWith
      (\p kern -> showsPrec p (toList kern))
      "fromList"

createKernFormat0 ::
  Int ->       -- nPairs
  ( forall s.
    MutablePrimArray s Int ->
    MutablePrimArray s Int ->
    ST s ()
  ) -> Kern
createKernFormat0 nPairs cont =
  runST do
    buf_leftrightPairs <- newPrimArray nPairs
    buf_values         <- newPrimArray nPairs
    cont buf_leftrightPairs buf_values
    leftrightPairs <- unsafeFreezePrimArray buf_leftrightPairs
    values         <- unsafeFreezePrimArray buf_values
    return (MkKernFormat0 leftrightPairs values)

mkLeftRightPair :: GlyphId -> GlyphId -> Int
mkLeftRightPair left right = unsafeShiftL left 16 .|. right

splitLeftRightPair :: Int -> (GlyphId, GlyphId)
splitLeftRightPair pair = (left, right)
  where
    right = word16_to_int (fromIntegral pair :: Word16)
    left  = word16_to_int (fromIntegral (unsafeShiftR pair 16) :: Word16)

lookupKern :: Kern -> GlyphId -> GlyphId -> Maybe Int
lookupKern (MkKernFormat0 leftrightPairs values) left right =
  if left < lrMaxBound && right < lrMaxBound then
    find_kern_value 0 nPairs
  else
    Nothing
  where
    lrMaxBound = 65536   -- 2^16

    nPairs = sizeofPrimArray leftrightPairs
    pair = mkLeftRightPair left right

    -- Find the kerning value for the glyph pair.
    find_kern_value ::
      Int ->            -- lower bound for the binary search (inclusive)
      Int ->            -- upper bound for the binary search (not inclusive)
      Maybe Int
    find_kern_value lower upper =
      if lower < upper then
        case compare pair leftrightPair of
          LT -> find_kern_value lower i
          EQ -> Just value
          GT -> find_kern_value (i+1) upper
      else
        Nothing
      where
        i = (lower + upper) `div` 2   -- the middle of the search range
        leftrightPair = indexPrimArray leftrightPairs i
        value         = indexPrimArray values i

readKern :: ByteString -> Kern
readKern bs =
  if version /= 0 || nTables /= 1 then
    throw UnsupportedTTF
  else
    -- We checked that nTables==1, so we read the first (and only) subtable
    -- from the start. If support for multiple subtables is added, we will
    -- need to iterate over subtables here.
    readKernSubtable bs_at_subtables
  where
    bs_at_kernHeader = expectRecord @S.KernHeader bs
    bs_at_subtables  = atEnd bs_at_kernHeader
    version = bs_at_kernHeader.version
    nTables = bs_at_kernHeader.nTables

readKernSubtable :: ByteString -> Kern
readKernSubtable bs =
  -- Reading the bits right-to-left:
  --   1     (1=horizontal, 0=vertical)
  --   0     (1=minimum, 0=kerning)
  --   0     (1=cross-stream, 0=normal)
  --   0     (1=override, 0=additive)
  --   0000  (reserved)
  --   00000000   format=0, a sorted list of kerning pairs and values
  case coverage of
    0b0000000000000001 -> readKernSubtableFormat0 bs_at_subtable
    _                  -> throw UnsupportedTTF
  where
    bs_at_kernSubtableHeader = expectRecord @S.KernSubtableHeader bs
    bs_at_subtable = atEnd bs_at_kernSubtableHeader
    coverage = bs_at_kernSubtableHeader.coverage

readKernSubtableFormat0 :: ByteString -> Kern
readKernSubtableFormat0 bs =
  createKernFormat0 nPairs \buf_leftrightPairs buf_values ->
    forIndices nPairs \i -> do
      let bs_at_kernPair = unsafeDropTo @S.KernPair i bs_at_kernPairs
          left  = word16_to_int bs_at_kernPair.left
          right = word16_to_int bs_at_kernPair.right
          value = int16_to_int bs_at_kernPair.value
          leftrightPair = mkLeftRightPair left right
      writePrimArray buf_leftrightPairs i leftrightPair
      writePrimArray buf_values i value
  where
    bs_at_kernSubtableHeader = expectRecord @S.KernSubtableHeaderFormat0 bs
    bs_at_kernPairs          =
      expectMinLength (sizeOfArr @S.KernPair nPairs) $
      atEnd bs_at_kernSubtableHeader
    nPairs = word16_to_int bs_at_kernSubtableHeader.nPairs