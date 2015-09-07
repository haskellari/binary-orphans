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
  ]

roundtrip :: (Eq a, Show a, Arbitrary a, Binary a) => Proxy a -> a -> Property
roundtrip _ x = x === decode (encode x)

