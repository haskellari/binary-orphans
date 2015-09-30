{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
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
import           Data.Binary
import           Data.Fixed
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import           Data.Hashable (Hashable)
import qualified Data.Monoid as Monoid
import qualified Data.Tagged as Tagged
import qualified Data.Time as Time

-- From other packages
#if !(MIN_VERSION_text(1,2,1))
import           Data.Text.Binary ()
#endif
import           Data.Vector.Binary ()

#if !(MIN_VERSION_scientific(0,3,4))
import qualified Data.Scientific as S
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


instance  (Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
  get = fmap HM.fromList get
  put = put . HM.toList

instance (Hashable v, Eq v, Binary v) => Binary (HS.HashSet v) where
  get = fmap HS.fromList get
  put = put . HS.toList

#if !(MIN_VERSION_scientific(0,3,4))
instance Binary S.Scientific where
  get = liftM2 S.scientific get get
  put s = put (S.coefficient s) >> put (S.base10Exponent s)
#endif

instance Binary b => Binary (Tagged.Tagged s b) where
  put = put . Tagged.unTagged
  get = fmap Tagged.Tagged get

instance Binary (Fixed a) where
  put (MkFixed a) = put a
  get = MkFixed `liftM` get

instance Binary Time.Day where
  get = fmap Time.ModifiedJulianDay get
  put = put . Time.toModifiedJulianDay

instance Binary Time.UniversalTime where
  get = fmap Time.ModJulianDate get
  put = put . Time.getModJulianDate

instance Binary Time.DiffTime where
  get = fmap Time.picosecondsToDiffTime get
  put = (put :: Pico -> Put)  . realToFrac

instance Binary Time.UTCTime where
  get = liftM2 Time.UTCTime get get
  put (Time.UTCTime d dt) = put d >> put dt

instance Binary Time.NominalDiffTime where
  get = fmap realToFrac (get :: Get Pico)
  put = (put :: Pico -> Put)  . realToFrac

instance Binary Time.TimeZone where
  get = liftM3 Time.TimeZone get get get
  put (Time.TimeZone m s n) = put m >> put s >> put n

instance Binary Time.TimeOfDay where
  get = liftM3 Time.TimeOfDay get get get
  put (Time.TimeOfDay h m s) = put h >> put m >> put s

instance Binary Time.LocalTime where
  get = liftM2 Time.LocalTime get get
  put (Time.LocalTime d tod) = put d >> put tod

-- Monoid

-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.Dual a)
-- | /Since: binary-orphans-0.1.1.0/
instance Binary Monoid.All
-- | /Since: binary-orphans-0.1.1.0/
instance Binary Monoid.Any
-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.Sum a)
-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.Product a)
-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.First a)
-- | /Since: binary-orphans-0.1.1.0/
instance Binary a => Binary (Monoid.Last a)
