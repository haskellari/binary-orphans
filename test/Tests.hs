{-# LANGUAGE CPP #-}
module Main (main) where

import Data.Binary
import Data.Binary.Orphans ()
import Data.Proxy
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Time (UTCTime, Day, DiffTime, NominalDiffTime, TimeZone, TimeOfDay, LocalTime)
import Data.Time.Clock.TAI (AbsoluteTime)
import Data.Monoid (Sum)
import Data.Text (Text)
import Data.CaseInsensitive (CI)
import Data.Monoid (Sum(..))
import Data.Semigroup (Min(..))

import qualified Data.CaseInsensitive as CI

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Roundtrip"
  [ QC.testProperty "HashMap"         $ roundtrip (Proxy :: Proxy (HashMap Int String))
  , QC.testProperty "HashSet"         $ roundtrip (Proxy :: Proxy (HashSet Int))
  , QC.testProperty "UTCTime"         $ roundtrip (Proxy :: Proxy UTCTime)
  , QC.testProperty "Day"             $ roundtrip (Proxy :: Proxy Day)
  , QC.testProperty "DiffTime"        $ roundtrip (Proxy :: Proxy DiffTime)
  , QC.testProperty "NominalDiffTime" $ roundtrip (Proxy :: Proxy NominalDiffTime)
  , QC.testProperty "TimeZone"        $ roundtrip (Proxy :: Proxy TimeZone)
  , QC.testProperty "TimeOfDay"       $ roundtrip (Proxy :: Proxy TimeOfDay)
  , QC.testProperty "LocalTime"       $ roundtrip (Proxy :: Proxy LocalTime)
  , QC.testProperty "AbsoluteTime"    $ roundtrip (Proxy :: Proxy AbsoluteTime)
  , QC.testProperty "CI Text"         $ roundtrip (Proxy :: Proxy (CI Text))
  , QC.testProperty "Sum Int"         $ roundtrip (Proxy :: Proxy (Sum Int))
  , QC.testProperty "Min Int"         $ roundtrip (Proxy :: Proxy (Min Int))
  ]

roundtrip :: (Eq a, Show a, Binary a) => Proxy a -> a -> Property
roundtrip _ x = x === decode (encode x)
