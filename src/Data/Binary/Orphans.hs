{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
#if MIN_VERSION_base(4,7,0)
#define HAS_FIXED_CONSTRUCTOR
#endif
#ifndef HAS_FIXED_CONSTRUCTOR
{-# LANGUAGE ScopedTypeVariables #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Orphans
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Provides orphan 'Binary' instances for types in various packages:
--
--   * aeson
--   * scientific (prior to scientific-0.3.4.0)
--   * semigroups
--   * tagged
--   * text (through text-binary, or text >= 1.2.1)
--   * time
--   * unordered-containers
--   * vector (through vector-binary-instances)
--
-- Also there is @'Binary' 'Fixed'@ instance.
module Data.Binary.Orphans (
  -- * Class re-export
  Binary(..),
  -- * Module re-export
  module Data.Binary,
  ) where

import           Control.Monad (liftM, liftM2, liftM3)
import qualified Data.Aeson as A
import           Data.Bits
import           Data.Binary
import qualified Data.CaseInsensitive as CI
import qualified Data.Fixed as Fixed
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Hashable as Hashable
import           Data.List (unfoldr, foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Tagged as Tagged
import qualified Data.Time as Time
import qualified Data.Time.Clock.TAI as Time
import qualified Data.Void as Void
import           Numeric.Natural

-- From other packages
#if !(MIN_VERSION_text(1,2,1))
import           Data.Text.Binary ()
#endif
import           Data.Vector.Binary ()

#if !(MIN_VERSION_scientific(0,3,4))
import qualified Data.Scientific as S
#endif

#if MIN_VERSION_time(1,8,0)
import qualified Data.Time.Clock.System as Time
#endif

instance Binary A.Value where
  get = do
    t <- get :: Get Int
    case t of
      0 -> fmap A.Object get
      1 -> fmap A.Array get
      2 -> fmap A.String get
      3 -> fmap A.Number get
      4 -> fmap A.Bool get
      5 -> return A.Null
      _ -> fail $ "Invalid Value tag: " ++ show t

  put (A.Object v) = put (0 :: Int) >> put v
  put (A.Array v)  = put (1 :: Int) >> put v
  put (A.String v) = put (2 :: Int) >> put v
  put (A.Number v) = put (3 :: Int) >> put v
  put (A.Bool v)   = put (4 :: Int) >> put v
  put A.Null       = put (5 :: Int)


instance  (Hashable.Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
  get = fmap HM.fromList get
  put = put . HM.toList

instance (Hashable.Hashable v, Eq v, Binary v) => Binary (HS.HashSet v) where
  get = fmap HS.fromList get
  put = put . HS.toList

#if MIN_VERSION_hashable(1,2,5)
instance (Hashable.Hashable a, Binary a) => Binary (Hashable.Hashed a) where
  get = fmap Hashable.hashed get
  put = put . Hashable.unhashed
#endif

#if !(MIN_VERSION_scientific(0,3,4))
instance Binary S.Scientific where
  get = liftM2 S.scientific get get
  put s = put (S.coefficient s) >> put (S.base10Exponent s)
#endif

instance Binary b => Binary (Tagged.Tagged s b) where
  put = put . Tagged.unTagged
  get = fmap Tagged.Tagged get

#if !MIN_VERSION_binary(0,8,0)
#ifdef HAS_FIXED_CONSTRUCTOR
instance Binary (Fixed.Fixed a) where
  put (Fixed.MkFixed a) = put a
  get = Fixed.MkFixed `liftM` get
#else
instance Fixed.HasResolution a => Binary (Fixed.Fixed a) where
  -- Using undefined :: Maybe a as a proxy, as Data.Proxy is introduced only in base-4.7
  put x = put (truncate (x * fromInteger (Fixed.resolution (undefined :: Maybe a))) :: Integer)
  get = (\x -> fromInteger x / fromInteger (Fixed.resolution (undefined :: Maybe a))) `liftM` get
#endif
#endif

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance Binary Time.Day where
  get = fmap Time.ModifiedJulianDay get
  put = put . Time.toModifiedJulianDay

instance Binary Time.UniversalTime where
  get = fmap Time.ModJulianDate get
  put = put . Time.getModJulianDate

instance Binary Time.DiffTime where
  get = fmap Time.picosecondsToDiffTime get
  put = (put :: Fixed.Pico -> Put)  . realToFrac

instance Binary Time.UTCTime where
  get = liftM2 Time.UTCTime get get
  put (Time.UTCTime d dt) = put d >> put dt

instance Binary Time.NominalDiffTime where
  get = fmap realToFrac (get :: Get Fixed.Pico)
  put = (put :: Fixed.Pico -> Put)  . realToFrac

instance Binary Time.TimeZone where
  get = liftM3 Time.TimeZone get get get
  put (Time.TimeZone m s n) = put m >> put s >> put n

instance Binary Time.TimeOfDay where
  get = liftM3 Time.TimeOfDay get get get
  put (Time.TimeOfDay h m s) = put h >> put m >> put s

instance Binary Time.LocalTime where
  get = liftM2 Time.LocalTime get get
  put (Time.LocalTime d tod) = put d >> put tod

-- | /Since: binary-orphans-0.1.4.0/
instance Binary Time.AbsoluteTime where
  get = fmap (flip Time.addAbsoluteTime Time.taiEpoch) get
  put = put . flip Time.diffAbsoluteTime Time.taiEpoch

#if MIN_VERSION_time(1,8,0)
-- | /Since: binary-orphans-0.1.7.0/
instance Binary Time.SystemTime where
    get = liftM2 Time.MkSystemTime get get
    put (Time.MkSystemTime s ns) = put s >> put ns
#endif

#if !MIN_VERSION_binary(0,8,4)

-------------------------------------------------------------------------------
-- Monoid
-------------------------------------------------------------------------------

-- | @since 0.1.1.0
instance Binary a => Binary (Monoid.Dual a) where
  get = fmap Monoid.Dual get
  put = put . Monoid.getDual

-- | /Since: binary-orphans-0.1.1.0/
instance Binary Monoid.All where
  get = fmap Monoid.All get
  put = put . Monoid.getAll

-- | /Since: binary-orphans-0.1.1.0/
instance Binary Monoid.Any where
  get = fmap Monoid.Any get
  put = put . Monoid.getAny

-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.Sum a) where
  get = fmap Monoid.Sum get
  put = put . Monoid.getSum

-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.Product a) where
  get = fmap Monoid.Product get
  put = put . Monoid.getProduct

-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.First a) where
  get = fmap Monoid.First get
  put = put . Monoid.getFirst

-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.Last a) where
  get = fmap Monoid.Last get
  put = put . Monoid.getLast

#if MIN_VERSION_base(4,8,0)
-- | /Since: binary-orphans-0.1.5.0/
instance Binary (f a) => Binary (Monoid.Alt f a) where
  get = fmap Monoid.Alt get
  put = put . Monoid.getAlt
#endif
#endif

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

#if !MIN_VERSION_binary(0,8,4) || !MIN_VERSION_base(4,9,0)
-- | /Since: binary-orphans-0.1.3.0/
instance Binary a => Binary (Semigroup.Min a) where
  get = fmap Semigroup.Min get
  put = put . Semigroup.getMin

-- | /Since: binary-orphans-0.1.3.0/
instance Binary a => Binary (Semigroup.Max a) where
  get = fmap Semigroup.Max get
  put = put . Semigroup.getMax

-- | /Since: binary-orphans-0.1.3.0/
instance Binary a => Binary (Semigroup.First a) where
  get = fmap Semigroup.First get
  put = put . Semigroup.getFirst

-- | /Since: binary-orphans-0.1.3.0/
instance Binary a => Binary (Semigroup.Last a) where
  get = fmap Semigroup.Last get
  put = put . Semigroup.getLast

-- | /Since: binary-orphans-0.1.3.0/
instance Binary a => Binary (Semigroup.Option a) where
  get = fmap Semigroup.Option get
  put = put . Semigroup.getOption

-- | /Since: binary-orphans-0.1.3.0/
instance Binary a => Binary (NE.NonEmpty a) where
  get = fmap NE.fromList get
  put = put . NE.toList

-- | /Since: binary-orphans-0.1.5.0/
instance Binary m => Binary (Semigroup.WrappedMonoid m) where
  get = fmap Semigroup.WrapMonoid get
  put = put . Semigroup.unwrapMonoid

-- | /Since: binary-orphans-0.1.5.0/
instance (Binary a, Binary b) => Binary (Semigroup.Arg a b) where
  get                     = liftM2 Semigroup.Arg get get
  put (Semigroup.Arg a b) = put a >> put b
#endif

-------------------------------------------------------------------------------
-- case-insensitive
-------------------------------------------------------------------------------

-- | /Since: binary-orphans-0.1.5.0/
instance (CI.FoldCase a, Binary a) => Binary (CI.CI a) where
  get = fmap CI.mk get
  put = put . CI.foldedCase

-------------------------------------------------------------------------------
-- void
-------------------------------------------------------------------------------

#if !MIN_VERSION_binary(0,8,0)
instance Binary Void.Void where
    put     = Void.absurd
    get     = fail "Binary.get @Void"
#endif

-------------------------------------------------------------------------------
-- nats
-------------------------------------------------------------------------------

#ifndef MIN_VERSION_nats
#define MIN_VERSION_nats(x,y,z) 0
#endif

#if !MIN_VERSION_binary(0,7,3) && !MIN_VERSION_nats(1,1,0)
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
