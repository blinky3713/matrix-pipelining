module Tests.Matrix.Pipelined where

import           Clash.Prelude    hiding (transpose)

import qualified Data.List        as L
import           Matrix.Pipelined
import           Test.Hspec

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

  it "Can multiply a matrix times a matrix using signals" $ do
      let input = fromList $ Nothing : Nothing : Just (m,m') : L.repeat Nothing
      Just mm' `shouldBe` takeFirstJust (sampleN 100 (mmmult2d (SNat @2) (SNat @2) (SNat @2) input))

takeFirstJust
  :: [Maybe a]
  -> Maybe a
takeFirstJust []              = Nothing
takeFirstJust (Just a : _)    = Just a
takeFirstJust (_ : rest)      = takeFirstJust rest
