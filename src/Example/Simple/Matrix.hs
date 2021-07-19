module Example.Simple.Matrix where

import Prelude
import Control.Monad

type Vector = [Int]

-- Row vectors
type Matrix = [Vector]

dot :: Vector -> Vector -> Int
dot a b = sum $ zipWith (*) a b

mvMult
  :: Matrix -- m :: (m x n) row vectors of dim n
  -> Vector -- v :: vector of dim n
  -> Vector -- m * v :: vector of dim m
mvMult m v = map (dot v) m

mmMult
  :: Matrix
  -> Matrix
  -> Matrix
mmMult m1 m2 =
  map (mvMult $ transpose m2) m1

transpose :: Matrix -> Matrix
transpose = foldr (zipWith (:)) (repeat [])
