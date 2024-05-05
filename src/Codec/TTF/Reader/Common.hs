{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.TTF.Reader.Common
  ( TTFException(..),
    UnsafeRead(..),
    forIndices,
    expectMinLength,
    word16_to_int,
    word32_to_int,
    int16_to_int,
    Word16,
    Word32,
    Int16,
    TaggedBytes(..),
    untagBytes,
    StaticSize(..),
    StaticOffset(..),
    atEnd,
    unsafeDropTo,
    sizeOfArr,
    expectRecord,
    expectRecordAtOffset,
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)
import Control.Exception (Exception, throw)
import Control.Monad (when)
import Data.Int (Int16)
import Data.Word (Word16, Word32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS.U
import qualified Data.ByteString.Internal as BS.I
import GHC.Ptr (Ptr(Ptr))
import GHC.Word (Word16(W16#), Word32(W32#), byteSwap16, byteSwap32)
import GHC.Int (Int(I#))
import GHC.Exts (indexWord16OffAddr#, indexWord32OffAddr#, plusAddr#)
import GHC.Records

data TTFException =
    MalformedTTF     -- | The font file does not adhere to the specification
  | UnsupportedTTF   -- | The font file uses a feature not supported by this implementation
  deriving Show

instance Exception TTFException

-- Safety condition: the ByteString must contain at least offset+2 bytes
unsafeReadWord16 :: Int -> ByteString -> Word16
unsafeReadWord16 offset bs = unsafeReadWord16_i offset 0 bs
{-# INLINE unsafeReadWord16 #-}

-- Safety condition: the ByteString must contain at least offset+i*2+2 bytes
unsafeReadWord16_i :: Int -> Int -> ByteString -> Word16
unsafeReadWord16_i (I# offset) (I# i) (BS.I.BS fptr _) =
  BS.I.accursedUnutterablePerformIO $
    BS.I.unsafeWithForeignPtr fptr $ \(Ptr addr) ->
      return (byteSwap16 (W16# (indexWord16OffAddr# (plusAddr# addr offset) i)))
{-# INLINE unsafeReadWord16_i #-}

-- Safety condition: the ByteString must contain at least offset+2 bytes
unsafeReadInt16 :: Int -> ByteString -> Int16
unsafeReadInt16 offset bs = fromIntegral (unsafeReadWord16 offset bs)
{-# INLINE unsafeReadInt16 #-}

-- Safety condition: the ByteString must contain at least offset+i*2+2 bytes
unsafeReadInt16_i :: Int -> Int -> ByteString -> Int16
unsafeReadInt16_i offset i bs = fromIntegral (unsafeReadWord16_i offset i bs)
{-# INLINE unsafeReadInt16_i #-}

-- Safety condition: the ByteString must contain at least offset+4 bytes
unsafeReadWord32 :: Int -> ByteString -> Word32
unsafeReadWord32 offset bs = unsafeReadWord32_i offset 0 bs
{-# INLINE unsafeReadWord32 #-}

-- Safety condition: the ByteString must contain at least offset+4*i+4 bytes
unsafeReadWord32_i :: Int -> Int -> ByteString -> Word32
unsafeReadWord32_i (I# offset) (I# i) (BS.I.BS fptr _) =
  BS.I.accursedUnutterablePerformIO $
    BS.I.unsafeWithForeignPtr fptr $ \(Ptr addr) ->
      return (byteSwap32 (W32# (indexWord32OffAddr# (plusAddr# addr offset) i)))
{-# INLINE unsafeReadWord32_i #-}

class UnsafeRead a where
  unsafeRead :: Int -> ByteString -> a
  unsafeReadAtIndex :: Int -> Int -> ByteString -> a

instance UnsafeRead Word16 where
  unsafeRead = unsafeReadWord16
  {-# INLINE unsafeRead #-}
  unsafeReadAtIndex = unsafeReadWord16_i
  {-# INLINE unsafeReadAtIndex #-}

instance UnsafeRead Word32 where
  unsafeRead = unsafeReadWord32
  {-# INLINE unsafeRead #-}
  unsafeReadAtIndex = unsafeReadWord32_i
  {-# INLINE unsafeReadAtIndex #-}

instance UnsafeRead Int16 where
  unsafeRead = unsafeReadInt16
  {-# INLINE unsafeRead #-}
  unsafeReadAtIndex = unsafeReadInt16_i
  {-# INLINE unsafeReadAtIndex #-}

forIndices :: forall m. Monad m => Int -> (Int -> m ()) -> m ()
forIndices n cont = go 0
  where
    go :: Int -> m ()
    go i = when (i < n) do
      cont i
      go (i+1)
{-# INLINE forIndices #-}

expectMinLength :: Int -> ByteString -> ByteString
expectMinLength n bs =
  if BS.length bs < n
  then throw MalformedTTF
  else bs

word16_to_int :: Word16 -> Int
word16_to_int = fromIntegral
{-# INLINE word16_to_int #-}

word32_to_int :: Word32 -> Int
word32_to_int = fromIntegral
{-# INLINE word32_to_int #-}

int16_to_int :: Int16 -> Int
int16_to_int = fromIntegral
{-# INLINE int16_to_int #-}

newtype TaggedBytes tag = TagBytes ByteString

untagBytes :: TaggedBytes tag -> ByteString
untagBytes (TagBytes bs) = bs

instance (UnsafeRead a, StaticOffset fld tag a) => HasField fld (TaggedBytes tag) a where
  getField (TagBytes bs) = unsafeRead (offsetTo @fld @tag) bs

type StaticSize :: Type -> Constraint
class StaticSize tag where
  sizeOf :: Int

type StaticOffset :: Symbol -> Type -> Type -> Constraint
class StaticOffset fld tag a | fld tag -> a where
  offsetTo :: Int

atEnd :: forall tag. StaticSize tag => TaggedBytes tag -> ByteString
atEnd (TagBytes bs) = BS.U.unsafeDrop (sizeOf @tag) bs
{-# INLINE atEnd #-}

unsafeDropTo :: forall tag. StaticSize tag => Int -> ByteString -> TaggedBytes tag
unsafeDropTo i bs = TagBytes @tag (BS.U.unsafeDrop (i * sizeOf @tag) bs)
{-# INLINE unsafeDropTo #-}

sizeOfArr :: forall tag. StaticSize tag => Int -> Int
sizeOfArr n = n * sizeOf @tag
{-# INLINE sizeOfArr #-}

expectRecord :: forall tag. StaticSize tag => ByteString -> TaggedBytes tag
expectRecord bs =
  if BS.length bs < sizeOf @tag
  then throw MalformedTTF
  else TagBytes @tag bs
{-# INLINE expectRecord #-}

expectRecordAtOffset :: forall tag. StaticSize tag => Int -> ByteString -> TaggedBytes tag
expectRecordAtOffset offset bs =
  if BS.length bs < (offset + sizeOf @tag)
  then throw MalformedTTF
  else TagBytes @tag (BS.U.unsafeDrop offset bs)
{-# INLINE expectRecordAtOffset #-}