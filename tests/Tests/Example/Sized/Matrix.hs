module Tests.Example.Sized.Matrix where

import Clash.Prelude hiding (transpose)

import qualified Data.List as L
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
      let m :: Matrix 3 3 Int
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
    let m :: Matrix 1 3 Int
        m = $(listToVecTH @Int [1,2,3]) :> Nil
        mT :: Matrix 3 1 Int
        mT = $(listToVecTH @Int [1]) :> $(listToVecTH @Int [2]) :> $(listToVecTH @Int [3]) :> Nil
    transpose m `shouldBe` mT

  it "Can take a transpose of a sized 3x2" $ do
    let m :: Matrix 2 3 Int
        m = $(listToVecTH @Int [1,2,3]) :>
              $(listToVecTH @Int [4,5,6]) :>
              Nil
        mT :: Matrix 3 2 Int
        mT = $(listToVecTH @Int [1,4]) :>
               $(listToVecTH @Int [2,5]) :>
               $(listToVecTH @Int [3,6]) :>
               Nil
    transpose m `shouldBe` mT

{-
|1 2 3 4|   |-1 0 1 0|   |-3 9 2 -1|
|2 3 4 0| x |1 1 1 1 | = |1 7 9 -1|
|3 4 0 1|   |0 1 1 -1|   |0 5 6 4|
|4,0,1,2|   |-1,1,-1,0|  |-6 3 3 -1
-}

  let m :: Matrix 4 4 Int
      m = $(listToVecTH @Int   [1,2,3,4]) :>
            $(listToVecTH @Int [2,3,4,0]) :>
            $(listToVecTH @Int [3,4,0,1]) :>
            $(listToVecTH @Int [4,0,1,2]) :>
            Nil
      m' :: Matrix 4 4 Int
      m' = $(listToVecTH @Int [-1,0,1,0]) :>
            $(listToVecTH @Int [1,1,1,1]) :>
            $(listToVecTH @Int [0,1,1,-1]) :>
            $(listToVecTH @Int [-1,1,-1,0]) :>
            Nil
      mm' :: Matrix 4 4 Int
      mm' = $(listToVecTH @Int [-3,9,2,-1]) :>
            $(listToVecTH @Int [1,7,9,-1]) :>
            $(listToVecTH @Int [0,5,6,4]) :>
            $(listToVecTH @Int [-6,3,3,-1]) :>
            Nil

  it "Can split and unsplit a matrix" $ do
    unsplitMatrix (splitMatrix (SNat @2) (SNat @2) m) `shouldBe` m

  it "Can multiply a matrix times a matrix using mmMult" $ do
      mmMult m m' `shouldBe` mm'

  it "Can multiply a matrix times a matrix using signals" $ do
      let input = fromList $ Nothing : Nothing : Just (m,m') : L.repeat Nothing
      Just mm' `shouldBe` takeFirstJust (sampleN 100 (mmmult2d (SNat @2) (SNat @2) (SNat @2) input))

takeFirstJust
  :: [Maybe a]
  -> Maybe a
takeFirstJust [] = Nothing
takeFirstJust (Just a : rest) = Just a
takeFirstJust (_ : rest) = takeFirstJust rest
