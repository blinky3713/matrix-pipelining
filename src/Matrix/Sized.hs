module Matrix.Sized where

import           Clash.Prelude hiding (transpose)

type Matrix m n a = Vec m (Vec n a) -- n x m matrix

dot
  :: forall n a.
     KnownNat n
  => 1 <= n
  => Num a
  => Vec n a
  -> Vec n a
  -> a
dot a b = sum $ zipWith (*) a b

vAdd
  :: forall a n.
     Num a
  => Vec n a
  -> Vec n a
  -> Vec n a
vAdd = zipWith (+)

mAdd
  :: forall a m n.
     Num a
  => Matrix m n a
  -> Matrix m n a
  -> Matrix m n a
mAdd = zipWith vAdd

mvMult
  :: forall m n a.
     KnownNat n
  => 1 <= n
  => Num a
  => Matrix m n a
  -> Vec n a
  -> Vec m a
mvMult m v = map (dot v) m

mmMult
  :: forall m n p a.
     KnownNat m
  => KnownNat p
  => Num a
  => 1 <= m
  => Matrix n m a
  -> Matrix m p a
  -> Matrix n p a
mmMult a b =
  map (mvMult $ transpose b) a

transpose
  :: forall m n a.
     KnownNat n
  => Num a
  => Matrix m n a -- matrix (n x m) of column vectors
  -> Matrix n m a -- matrix (m x n) of column vectors
transpose Nil = repeat Nil
transpose (Cons as rest) =
  let m = transpose rest
  in zipWith (:>) as m

{-
[ [a,b,c,d]
, [d,e,f,g]
, [i,j,k,l]
, [m,n,o,p]
]

| a b c d |
| e f g h |
| i j k l |
| m n o p |


[ [[a,b],[e,f]]
, [[c,d],[g,h]]
, [[i,j],[k,l]]
, [[m,n],[o,p]]
]

-> ||a b| |c d||
   ||e f| |g h||
   |           |
   ||i j| |k l||
   ||m n| |o p||
-}


splitMatrix
  :: forall aa_m aa_n sm sn m n a.
     KnownNat aa_m
  => KnownNat aa_n
  => KnownNat sm
  => KnownNat sn
  => 1 <= aa_m
  => 1 <= sm
  => (aa_m * sm) ~ m
  => (aa_n * sn) ~ n
  => SNat sm
  -> SNat sn
  -> Matrix m n a
  -> Matrix aa_m aa_n (Matrix sm sn a)
splitMatrix sm sn =
  map (traverse $ unconcat sn) . unconcat sm

unsplitMatrix
  :: forall aa_m aa_n sm sn m n a.
     KnownNat aa_m
  => KnownNat aa_n
  => KnownNat sm
  => KnownNat sn
  => 1 <= aa_n
  => (aa_m * sm) ~ m
  => (aa_n * sn) ~ n
  => Matrix aa_m aa_n (Matrix sm sn a)
  -> Matrix m n a
unsplitMatrix = concatMap (map concat . sequenceA)

replaceMatrixElement
  :: forall m n a.
     KnownNat m
  => KnownNat n
  => Matrix m n a
  -> Index m
  -> Index n
  -> a
  -> Matrix m n a
replaceMatrixElement a rowIndex colIndex _sub =
  let sub' = replace colIndex _sub (a !! rowIndex)
  in replace rowIndex sub' a

--------------------------------------------------------------------------------

type Counter aa_n bb_n aa_m = (Index aa_n, Index bb_n, Index aa_m)

mmmult2d
  :: forall a_m a_n b_m b_n aa_m aa_n bb_m bb_n aa_sm aa_sn bb_sm bb_sn.
     SystemClockResetEnable
  => KnownNat a_m
  => KnownNat a_n
  => KnownNat b_m
  => KnownNat b_n
  => KnownNat aa_m
  => KnownNat aa_n
  => KnownNat bb_m
  => KnownNat bb_n
  => KnownNat aa_sm
  => KnownNat aa_sn
  => KnownNat bb_sm
  => KnownNat bb_sn
  => 1 <= a_m
  => 1 <= a_n
  => 1 <= b_m
  => 1 <= b_n
  => 1 <= aa_m
  => 1 <= aa_n
  => 1 <= bb_m
  => 1 <= bb_n
  => a_n ~ b_m

  -- Constrain submatrices:
  => a_m ~ (aa_m * aa_sm)
  => a_n ~ (aa_n * aa_sn)
  => b_m ~ (bb_m * bb_sm)
  => b_n ~ (bb_n * bb_sn)
  => 1 <= aa_sm
  => 1 <= aa_sn
  => 1 <= bb_sm
  => 1 <= bb_sn
  => bb_sm ~ aa_sn

  -- Allow user to pass submatrix sizes:
  => SNat aa_m
  -- ^ Number of rows in submatrix of AA
  -> SNat aa_sn
  -- ^ Number of columns in submatrix of AA
  -> SNat bb_n
  -- ^ Number of columns in submatrix of BB

  -- Matrices to multiply:
  -> Signal System (Maybe (Matrix a_m a_n Int, Matrix b_m b_n Int))

  -- Result returned after calculating for a while:
  -> Signal System (Maybe (Matrix a_m b_n Int))
mmmult2d _ aa_sn _ ab =
  let
      splitAB
        :: (Matrix a_m a_n Int, Matrix b_m b_n Int)
        -> (Matrix aa_m aa_n (Matrix aa_sm aa_sn Int), Matrix bb_m bb_n (Matrix bb_sm bb_sn Int))
      splitAB (a, b) =
        ( splitMatrix (SNat @aa_sm) aa_sn a
        , splitMatrix (SNat @bb_sm) (SNat @bb_sn) b
        )
  in mealy mmmult2dmealy initialState (fmap splitAB <$> ab)
  where
    nullMatrix :: Matrix aa_m bb_n (Matrix aa_sm bb_sn Int)
    nullMatrix = repeat $ repeat $ repeat $ repeat 0

    initialState
      :: ( Maybe (Matrix aa_m aa_n (Matrix aa_sm aa_sn Int), Matrix bb_m bb_n (Matrix bb_sm bb_sn Int))
         , Counter aa_n bb_n aa_m
         , Matrix aa_m bb_n (Matrix aa_sm bb_sn Int)
         )
    initialState = (Nothing, minBound, nullMatrix)

    incCounterWithWrap ctr@(a,b,c)
      | ctr == maxBound = minBound
      | a == maxBound && b == maxBound = (0,0,c+1)
      | a == maxBound = (0,b+1,c)
      | otherwise = (a+1,b,c)

    mmmult2dmealy (Nothing, _, _) Nothing = ((Nothing, minBound, nullMatrix), Nothing)
    mmmult2dmealy _ matrices@(Just _) = ((matrices, minBound, nullMatrix), Nothing)
    mmmult2dmealy (matrices@(Just (matrixAA, matrixBB)), counter, matrixRR) _ = (newState, output)
      where
        done = counter == maxBound
        counter' = incCounterWithWrap counter

        newState
          | done = initialState
          | otherwise = (matrices, counter', matrixRR')

        output
          | done = Just $ unsplitMatrix matrixRR'
          | otherwise = Nothing

        (aColI, _, aRowI) = counter
        (bRowI, bColI, _) = counter
        (_, rColI, rRowI) = counter

        subA :: Matrix aa_sm aa_sn Int
        subA = matrixAA !! aRowI !! aColI

        subB :: Matrix bb_sm bb_sn Int
        subB = matrixBB !! bRowI !! bColI

        subR :: Matrix aa_sm bb_sn Int
        subR = matrixRR !! rRowI !! rColI

        subR' = subR `mAdd` mmMult subA subB

        matrixRR' :: Matrix aa_m bb_n (Matrix aa_sm bb_sn Int)
        matrixRR' = replaceMatrixElement matrixRR rRowI rColI subR'
