{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE PolyKinds           #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Binary.Orphans () where

import Data.Binary

import Control.Monad (replicateM)

#if MIN_VERSION_base(4,18,0)
import Data.Tuple (Solo (MkSolo))
#elif MIN_VERSION_base(4,16,0)
import Data.Tuple (Solo (Solo))
#define MkSolo Solo
#else
import Data.Tuple.Solo (Solo (MkSolo))
#endif

import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import GHC.Exts
       (Int (..), indexWord8Array#, newByteArray#, sizeofByteArray#,
       unsafeFreezeByteArray#, writeWord8Array#)
import GHC.ST          (ST (..), runST)
import GHC.Word        (Word8 (..))

-------------------------------------------------------------------------------
-- future-binary
-------------------------------------------------------------------------------

-- | @since 1.0.2
instance Binary a => Binary (Solo a) where
  put (MkSolo x) = put x
  get = fmap MkSolo get

-- | @since 1.0.3
instance Binary ByteArray where
  put ba = put maxI >> go 0
    where
      maxI :: Int
      maxI = sizeofByteArray ba

      go :: Int -> Put
      go i | i < maxI  = put (indexByteArray ba i) >> go (i + 1)
           | otherwise = return ()

  get = do
    len <- get
    xs  <- replicateM len get
    return (byteArrayFromListN len xs)

{-# INLINE sizeofByteArray #-}
sizeofByteArray :: ByteArray -> Int
sizeofByteArray (ByteArray ba) = I# (sizeofByteArray# ba)

{-# INLINE indexByteArray #-}
indexByteArray :: ByteArray -> Int -> Word8
indexByteArray (ByteArray ba) (I# i) = W8# (indexWord8Array# ba i)

{-# INLINE byteArrayFromListN #-}
byteArrayFromListN :: Int -> [Word8] -> ByteArray
byteArrayFromListN len xs = runST $ do
    mba <- newByteArray len
    go mba 0 xs
    unsafeFreezeByteArray mba
  where
    go :: MutableByteArray s -> Int -> [Word8] -> ST s ()
    go mba i ys
        | i < len = case ys of
            []   -> writeWord8Array mba i 0 >> go mba (i + 1) ys
            z:zs -> writeWord8Array mba i z >> go mba (i + 1) zs

        | otherwise = return ()

{-# INLINE newByteArray #-}
newByteArray :: Int -> ST s (MutableByteArray s)
newByteArray (I# len) = ST $ \s -> case newByteArray# len s of
    (# s', mba #) -> (# s', MutableByteArray mba #)

{-# INLINE unsafeFreezeByteArray #-}
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray mba) = ST $ \s -> case unsafeFreezeByteArray# mba s of
    (# s', ba #) -> (# s', ByteArray ba #)

{-# INLINE writeWord8Array #-}
writeWord8Array :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeWord8Array (MutableByteArray mba) (I# i) (W8# w) = ST $ \s -> case writeWord8Array# mba i w s of
    s' -> (# s', () #)
