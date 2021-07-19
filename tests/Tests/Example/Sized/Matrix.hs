module Tests.Example.Sized.Matrix where

import Clash.Prelude hiding (transpose)

import Test.Hspec
import Test.QuickCheck
import Example.Sized.Matrix

spec :: IO ()
spec = hspec $ do
  describe "Basic Sized Operations" $ do
    it "Can take a sized dot product" $ do
      let a :: Vec 3 Int
          a = 1 :> 2 :> 3 :> Nil
          b = 4 :> 5 :> 6 :> Nil
      dot a b `shouldBe` 32

    it "Can multiply a matrix times a sized vector" $ do
      let m :: Matrix 3 3
          m = $(listToVecTH @Int [1,2,3]) :>
                $(listToVecTH @Int [2,3,0]) :>
                $(listToVecTH @Int [3,0,1]) :>
                Nil
          v :: Vec 3 Int
          v = $(listToVecTH @Int [-1,-1,1])
          mv :: Vec 3 Int
          mv = $(listToVecTH @Int [0,-5,-2])
      mvMult m v `shouldBe` mv

  it "Can take a transpose of a sized row vector" $ do
    let m :: Matrix 1 3
        m = $(listToVecTH @Int [1,2,3]) :> Nil
        mT :: Matrix 3 1
        mT = $(listToVecTH @Int [1]) :> $(listToVecTH @Int [2]) :> $(listToVecTH @Int [3]) :> Nil
    transpose m `shouldBe` mT

  it "Can take a transpose of a sized 3x2" $ do
    let m :: Matrix 2 3
        m = $(listToVecTH @Int [1,2,3]) :>
              $(listToVecTH @Int [4,5,6]) :>
              Nil
        mT :: Matrix 3 2
        mT = $(listToVecTH @Int [1,4]) :>
               $(listToVecTH @Int [2,5]) :>
               $(listToVecTH @Int [3,6]) :>
               Nil
    transpose m `shouldBe` mT

{-
|1 2 3|   |-1 0 1|   |1  5 6|
|2 3 0| x |1  1 1| = |1  3 5|
|3 0 1|   |0  1 1|   |-3 1 4|
-}

  it "Can multiply a matrix times a matrix" $ do
      let m :: Matrix 3 3
          m = $(listToVecTH @Int [1,2,3]) :>
                $(listToVecTH @Int [2,3,0]) :>
                $(listToVecTH @Int [3,0,1]) :>
                Nil
          m' :: Matrix 3 3
          m' = $(listToVecTH @Int [-1,0,1]) :>
                $(listToVecTH @Int [1,1,1]) :>
                $(listToVecTH @Int [0,1,1]) :>
                Nil
          mm' :: Matrix 3 3
          mm' = $(listToVecTH @Int [1,5,6]) :>
                $(listToVecTH @Int [1,3,5]) :>
                $(listToVecTH @Int [-3,1,4]) :>
                Nil
      mmMult m m' `shouldBe` mm'

