module Main (main) where

import Data.Array.Byte           (ByteArray)
import Data.Binary               (Binary, decode, encode)
import Data.Binary.Orphans ()
import Data.Monoid               (Sum)
import Data.Proxy                (Proxy (..))
import Data.Semigroup            (Min (..))
import Data.Tuple.Solo           (Solo (..))
import Numeric.Natural           (Natural)
import Test.QuickCheck           (Property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty                (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck     (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Roundtrip"
  [ testProperty "Natural"         $ roundtrip (Proxy :: Proxy Natural)
  , testProperty "Sum Int"         $ roundtrip (Proxy :: Proxy (Sum Int))
  , testProperty "Min Int"         $ roundtrip (Proxy :: Proxy (Min Int))
  , testProperty "Solo Int"        $ roundtrip (Proxy :: Proxy (Solo Int))
  , testProperty "ByteArray"       $ roundtrip (Proxy :: Proxy ByteArray)
  ]

roundtrip :: (Eq a, Show a, Binary a) => Proxy a -> a -> Property
roundtrip _ x = x === decode (encode x)
