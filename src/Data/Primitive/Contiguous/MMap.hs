{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}

module Data.Primitive.Contiguous.MMap
  ( Mode(..)
  , FileSlice(..)
  , unsafeMMapMutable
  , unsafeMMap
  , writeMMap
  ) where

import System.IO.MMap
import Foreign.Storable
import Data.Primitive.Contiguous
import Data.Int (Int64)
import Control.Monad.Primitive
import GHC.Ptr
import GHC.ForeignPtr
import Foreign.ForeignPtr

-- | A slice of a file to mmap.
data FileSlice
  = EntireFile -- ^ mmap the entire file
  | FileSlice  -- ^ mmap a slice of the file
      {-# UNPACK #-} !Int64 -- ^ offset into file
      {-# UNPACK #-} !Int   -- ^ element count

-- | Map a file into memory as a mutable array.
unsafeMMapMutable :: forall arr a. (Contiguous arr, Element arr a, Storable a)
  => FilePath -- ^ path of the file to mmap
  -> Mode -- ^ mapping mode
  -> FileSlice -- ^ slice of the file to mmap
  -> IO (Mutable arr RealWorld a)
unsafeMMapMutable path mode range = do
  (foreignPtr, offset, sz) <- mmapFileForeignPtr path mode $ case range of
    EntireFile -> Nothing
    FileSlice start len -> Just (start, len * sizeOf (undefined :: a))
  unsafeFromForeignPtrMutable foreignPtr offset (sz `div` sizeOf (undefined :: a))
{-# inline unsafeMMapMutable #-}

-- | Map a file into memory as an immutable array.
--   This operation is performed in 'ReadOnly' mode.
unsafeMMap :: forall arr a. (Contiguous arr, Element arr a, Storable a)
  => FilePath -- ^ path of the file to mmap
  -> FileSlice -- ^ slice of the file to mmap
  -> IO (arr a)
unsafeMMap path range = do
  (foreignPtr, offset, sz) <- mmapFileForeignPtr path ReadOnly $ case range of
    EntireFile -> Nothing
    FileSlice start len -> Just (start, len * sizeOf (undefined :: a))
  unsafeFromForeignPtr foreignPtr offset (sz `div` sizeOf (undefined :: a))
{-# inline unsafeMMap #-}

-- | Write a file from an array.
-- 
--   /Note/: Be careful with existing files, parts behind the mapped range
--   will stay as they are before the write operation.
writeMMap :: forall arr a. (Contiguous arr, Element arr a, Storable a)
  => FilePath -- ^ path of the file to map
  -> arr a -- ^ array to write
  -> IO ()
writeMMap path src = do
  let !sz = size src
  target <- unsafeMMapMutable path ReadWriteEx (FileSlice 0 sz)
  copy target 0 src 0 sz
{-# inline writeMMap #-}

unsafeFromForeignPtrMutable :: forall arr a. (Contiguous arr, Element arr a, Storable a)
  => ForeignPtr a -- ^ foreign ptr to the array
  -> Int -- ^ offset from the pointer, in number of elements
  -> Int -- ^ number of elements to marshal
  -> IO (Mutable arr RealWorld a)
unsafeFromForeignPtrMutable fp off elems = withForeignPtr fp $ \ptr -> do
  let !sz = elems
  marr <- new sz
  let go !ix !p = if ix < sz
        then do
          e <- peek p
          write marr ix e
          go (ix+1) (plusPtr p (1 * sizeOf (undefined :: a)))
        else do
          pure ()
  go 0 (plusPtr ptr (off * sizeOf (undefined :: a)))
  pure marr
{-# inline unsafeFromForeignPtrMutable #-}

unsafeFromForeignPtr :: forall arr a. (Contiguous arr, Element arr a, Storable a)
  => ForeignPtr a -- ^ foreign ptr to the array
  -> Int -- ^ offset from the pointer
  -> Int -- ^ number of elements to marshal
  -> IO (arr a)
unsafeFromForeignPtr fp off elems = do
  marr <- unsafeFromForeignPtrMutable fp off elems
  freeze marr 0 elems
{-# inline unsafeFromForeignPtr #-} 
