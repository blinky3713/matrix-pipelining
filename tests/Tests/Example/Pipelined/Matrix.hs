module Tests.Example.Pipelined.Matrix where

import Clash.Prelude hiding (transpose)

import qualified Data.List as L
import Test.Hspec
import Test.QuickCheck
import Example.Pipelined.Matrix

spec :: IO ()
spec = hspec $ do
  describe "Basic Pipelined Operations" $ do
    it "Can take a pipelined dot product" $ do
      let a,b :: DSignal System 0 (Vec 3 Int)
          a = pure (1 :> 2 :> 3 :> Nil)
          b = pure (4 :> 5 :> 6 :> Nil)
          p :: Int
          p = L.head $ L.dropWhile (== 0) $ sampleN 10 (toSignal $ dotf a b)
      p `shouldBe` 32
