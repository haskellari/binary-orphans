{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds           #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}
module Data.Binary.Orphans () where

import Data.Binary
import Data.Binary.Put

import           Control.Applicative   (Alternative (..))
import           Control.Monad
                 (MonadPlus (..), liftM, liftM2, replicateM)
import qualified Control.Monad.Fail    as Fail
import           Data.Bits             (Bits, shiftL, shiftR, (.|.))
import           Data.Complex          (Complex (..))
import qualified Data.Fixed            as Fixed
import           Data.Functor.Identity (Identity (..))
import           Data.List             (foldl', unfoldr)
import qualified Data.List.NonEmpty    as NE
import qualified Data.Monoid           as Monoid
import           Data.Semigroup        ((<>))
import qualified Data.Semigroup        as Semigroup
import           Data.Version          (Version (..))
import           Data.Void             (Void, absurd)
import           GHC.Fingerprint       (Fingerprint (..))
import           Numeric.Natural       (Natural)

#if MIN_VERSION_base(4,16,0)
import Data.Tuple (Solo (..))
#else
import Data.Tuple.Solo (Solo (..))
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import GHC.Exts
       (Int (..), indexWord8Array#, newByteArray#, sizeofByteArray#,
       unsafeFreezeByteArray#, writeWord8Array#)
import GHC.ST          (ST (..), runST)
import GHC.Word        (Word8 (..))
#endif

-------------------------------------------------------------------------------
-- binary-0.7.1.0
-------------------------------------------------------------------------------

#if !(MIN_VERSION_binary(0,7,1))

-------------------------------------------------------------------------------
-- Add MonadPlus instance for Get.

-- Not implementable, as far as I can see

#endif

-------------------------------------------------------------------------------
-- binary-0.7.3.0
-------------------------------------------------------------------------------

#if !(MIN_VERSION_binary(0,7,3) )

-------------------------------------------------------------------------------
-- Add Binary instance for Natural (only with base > 4.8).

#ifndef MIN_VERSION_nats
#define MIN_VERSION_nats(x,y,z) 0
#endif

#if !(MIN_VERSION_nats(1,1,0))
-- Fixed-size type for a subset of Natural
type NaturalWord = Word64

-- | /Since: 0.7.3.0/
instance Binary Natural where
    {-# INLINE put #-}
    put n | n <= hi =
        putWord8 0
        >> put (fromIntegral n :: NaturalWord)  -- fast path
     where
        hi = fromIntegral (maxBound :: NaturalWord) :: Natural

    put n =
        putWord8 1
        >> put (unroll (abs n))         -- unroll the bytes

    {-# INLINE get #-}
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get NaturalWord)
            _ -> do bytes <- get
                    return $! roll bytes


--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll   = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b
#endif

#endif

-------------------------------------------------------------------------------
-- binary-0.7.6.0
-------------------------------------------------------------------------------

#if !MIN_VERSION_binary(0,7,6)

-------------------------------------------------------------------------------
-- Added binary instance for GHC.Fingerprint (from GHC >= 7.4).

instance Binary Fingerprint where
    put (Fingerprint x1 x2) = put x1 <> put x2
    get = do
        x1 <- get
        x2 <- get
        return $! Fingerprint x1 x2

#endif

-------------------------------------------------------------------------------
-- binary-0.8.0.0
-------------------------------------------------------------------------------

#if !(MIN_VERSION_binary(0,8,0))

-------------------------------------------------------------------------------
-- Added binary instance for Version from Data.Version.

instance Binary Version where
    put (Version br tags) = put br <> put tags
    get = liftM2 Version get get

-------------------------------------------------------------------------------
-- Added binary instance for Void from GHC 7.10.1.

instance Binary Void where
    put     = absurd
    get     = fail "get @Void"

-------------------------------------------------------------------------------
-- Added binary instance for (Data.Fixed a) from GHC 7.8.1.

#if MIN_VERSION_base(4,7,0)
instance Binary (Fixed.Fixed a) where
  put (Fixed.MkFixed a) = put a
  get = Fixed.MkFixed `fmap` get
#else
instance Fixed.HasResolution a => Binary (Fixed.Fixed a) where
  -- Using undefined :: Maybe a as a proxy, as Data.Proxy is introduced only in base-4.7
  put x = put (truncate (x * fromInteger (Fixed.resolution (undefined :: Maybe a))) :: Integer)
  get = (\x -> fromInteger x / fromInteger (Fixed.resolution (undefined :: Maybe a))) `fmap` get
#endif

#endif

-------------------------------------------------------------------------------
-- Added semigroup instance for Data.Binary.Builder from GHC 8.0.

-- in semigroups

-------------------------------------------------------------------------------
-- binary-0.8.2.0
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- When using GHC >= 8, Data.Binary.Get.Get implements MonadFail and delegates its fail to MonadFail.fail.

#if !(MIN_VERSION_binary(0,8,2)) || !(MIN_VERSION_base(4,9,0))
instance Fail.MonadFail Get where
    -- this is ok, as if old base: Prelude.fail is Monad's fail
    fail = Prelude.fail
#endif

-------------------------------------------------------------------------------
-- binary-0.8.3
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Add Monoid and Semigroup instance for Put.

#if !(MIN_VERSION_binary(0,8,3)) || !(MIN_VERSION_base(4,9,0))
instance Semigroup.Semigroup Put where
    (<>) = (>>)
#endif

#if !(MIN_VERSION_binary(0,8,3))
instance Monoid.Monoid Put where
    mempty = return ()
    mappend = (<>)

#endif

-------------------------------------------------------------------------------
-- Add Binary instance for Complex a.

#if !(MIN_VERSION_binary(0,8,3))

instance Binary a => Binary (Complex a) where
    {-# INLINE put #-}
    put (r :+ i) = put (r, i)
    {-# INLINE get #-}
    get = fmap (\(r,i) -> r :+ i) get

#endif

-------------------------------------------------------------------------------
-- binary-0.8.4
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Add Binary instances for datatypes in Data.Monoid/Data.Semigroup
-- https://github.com/kolmodin/binary/pull/114

#if !MIN_VERSION_binary(0,8,4)

instance Binary a => Binary (Monoid.Dual a) where
  get = fmap Monoid.Dual get
  put = put . Monoid.getDual

instance Binary Monoid.All where
  get = fmap Monoid.All get
  put = put . Monoid.getAll

instance Binary Monoid.Any where
  get = fmap Monoid.Any get
  put = put . Monoid.getAny

instance Binary a => Binary (Monoid.Sum a) where
  get = fmap Monoid.Sum get
  put = put . Monoid.getSum

instance Binary a => Binary (Monoid.Product a) where
  get = fmap Monoid.Product get
  put = put . Monoid.getProduct

instance Binary a => Binary (Monoid.First a) where
  get = fmap Monoid.First get
  put = put . Monoid.getFirst

instance Binary a => Binary (Monoid.Last a) where
  get = fmap Monoid.Last get
  put = put . Monoid.getLast

#if MIN_VERSION_base(4,8,0)
instance Binary (f a) => Binary (Monoid.Alt f a) where
  get = fmap Monoid.Alt get
  put = put . Monoid.getAlt
#endif

#endif

------------------------------------------------------------------------
-- Data.Semigroup datatypes

#if !MIN_VERSION_binary(0,8,4) || !MIN_VERSION_base(4,9,0)

instance Binary a => Binary (Semigroup.Min a) where
  get = fmap Semigroup.Min get
  put = put . Semigroup.getMin

instance Binary a => Binary (Semigroup.Max a) where
  get = fmap Semigroup.Max get
  put = put . Semigroup.getMax

instance Binary a => Binary (Semigroup.First a) where
  get = fmap Semigroup.First get
  put = put . Semigroup.getFirst

instance Binary a => Binary (Semigroup.Last a) where
  get = fmap Semigroup.Last get
  put = put . Semigroup.getLast

instance Binary a => Binary (Semigroup.Option a) where
  get = fmap Semigroup.Option get
  put = put . Semigroup.getOption

instance Binary m => Binary (Semigroup.WrappedMonoid m) where
  get = fmap Semigroup.WrapMonoid get
  put = put . Semigroup.unwrapMonoid

instance (Binary a, Binary b) => Binary (Semigroup.Arg a b) where
  get                     = liftM2 Semigroup.Arg get get
  put (Semigroup.Arg a b) = put a <> put b

instance Binary a => Binary (NE.NonEmpty a) where
  get = do
      x <- get
      case x of
          []     -> fail "empty NonEmpty"
          (x:xs) -> return (x NE.:| xs)
  put = put . NE.toList

#endif

-------------------------------------------------------------------------------
-- binary-0.8.5.0
-------------------------------------------------------------------------------

#if !(MIN_VERSION_binary(8,6,0))

-------------------------------------------------------------------------------
-- Typeable TypeReps
-- Add Binary instances for Typeable TypeReps
-- https://github.com/kolmodin/binary/pull/131

-- TODO

#endif

-------------------------------------------------------------------------------
-- binary-0.8.6.0
-------------------------------------------------------------------------------

#if !(MIN_VERSION_binary(8,6,0))

-------------------------------------------------------------------------------
-- Add binary instance for Data.Functor.Identity from base,
-- https://github.com/kolmodin/binary/pull/146

#if !MIN_VERSION_base(4,8,0)
instance Binary a => Binary (Identity a) where
  put (Identity x) = put x
  get = fmap Identity get
#endif

#endif

-------------------------------------------------------------------------------
-- future-binary
-------------------------------------------------------------------------------

-- | @since 1.0.2
instance Binary a => Binary (Solo a) where
  put (Solo x) = put x
  get = fmap Solo get

#if MIN_VERSION_base(4,9,0)
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
#endif
