module Example.Sized.Matrix where

import Clash.Prelude hiding (transpose)

type Matrix m n = Vec m (Vec n Int) -- n x m matrix

dot
  :: KnownNat n
  => 1 <= n
  => Vec n Int
  -> Vec n Int
  -> Int
dot a b = sum $ zipWith (*) a b


mvMult
  :: KnownNat n
  => 1 <= n
  => Matrix m n
  -> Vec n Int
  -> Vec m Int
mvMult m v = map (dot v) m

mmMult
  :: KnownNat m
  => KnownNat p
  => 1 <= m
  => Matrix n m
  -> Matrix m p
  -> Matrix n p
mmMult a b =
  map (mvMult $ transpose b) a

transpose
  :: KnownNat n
  => Matrix m n -- matrix (n x m) of column vectors
  -> Matrix n m -- matrix (m x n) of column vectors
transpose Nil = repeat Nil
transpose (Cons as rest) =
  let m = transpose rest
  in zipWith (:>) as m
