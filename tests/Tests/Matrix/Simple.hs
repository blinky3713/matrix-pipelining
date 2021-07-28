module Tests.Matrix.Simple where

import           Prelude

import           Matrix.Simple
import           Test.Hspec
import           Test.QuickCheck

spec :: IO ()
spec = hspec $ do
  describe "Basic Operations" $ do
    it "Can take a dot product" $ do
      let a = [1,2,3]
          b = [4,5,6]
      dot a b `shouldBe` 32
    it "Can multiply a matrix times a vector" $ do
      let m = [ [1,2,3]
              , [2,3,0]
              , [3,0,1]
              ]
          v = [-1,-1,1]
          mv = [0,-5,-2]
      mvMult m v `shouldBe` mv

  it "Can take a transpose of a row vector" $ do
    transpose [[1,2,3]] `shouldBe` [[1],[2],[3]]

  it "Can take a transpose of a 3x2" $ do
    let m = [ [1,2,3]
            , [4,5,6]
            ]
        mT = [ [1,4]
             , [2,5]
             , [3,6]
             ]
    transpose m `shouldBe` mT

{-
|1 2 3|   |-1 0 1|   |1  5 6|
|2 3 0| x |1  1 1| = |1  3 5|
|3 0 1|   |0  1 1|   |-3 1 4|
-}

  it "Can multiply a matrix times a matrix" $ do
      let m = [ [1,2,3]
              , [2,3,0]
              , [3,0,1]
              ]
          m' = [ [-1,0,1]
               , [1,1,1]
               , [0,1,1]
               ]
          mm' = [ [1,5,6]
                , [1,3,5]
                , [-3,1,4]
                ]
      mmMult m m' `shouldBe` mm'
