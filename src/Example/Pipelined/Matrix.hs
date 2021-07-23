module Example.Pipelined.Matrix
  ( module Example.Pipelined.Matrix
  , Matrix
  ) where

import           Clash.Prelude               hiding (transpose)
import qualified Clash.Signal.Delayed.Bundle as DBundle
import           Data.Maybe                  (fromJust)
import           Example.Sized.Matrix        (Matrix, replaceMatrixElement,
                                              splitMatrix, transpose,
                                              unsplitMatrix)


multiplyAdd
  :: Num a
  => Signal System (a,a)
  -> Signal System a
  -> Signal System a
multiplyAdd ab c =
  a * b + c
  where
    (a,b) = unbundle ab

dot
  :: forall n a.
     Num a
  => KnownNat n
  => Signal System (Vec n a)
  -> Signal System (Vec n a)
  -> Signal System a
dot a b = foldr multiplyAdd (pure 0) ab
  where
    ab :: Vec n (Signal System (a, a))
    ab =  unbundle $ zip <$> a <*> b



dotf
  :: forall dom n d a.
     HiddenClockResetEnable dom
  => KnownNat n
  => KnownNat d
  => NFDataX a
  => Num a
  => DSignal dom d (Vec n a)
  -> DSignal dom d (Vec n a)
  -> DSignal dom (d + n) a
dotf a b =
  foldrp @_ @_ @_ @d @n
    (\(_a,_b) c -> _a * _b + c)
    0
    (0,0)
    (DBundle.unbundle $ zip <$> a <*> b)
    (pure 0)

--------------------------------------------------------------------------------

type Counter a_n b_n a_m aa_sm bb_sn =
  (Index a_n, Index b_n, Index a_m, Index aa_sm, Index bb_sn)

succWrap
  :: forall  a_n b_n a_m aa_sm bb_sn.
     KnownNat a_n
  => KnownNat b_n
  => KnownNat a_m
  => KnownNat aa_sm
  => KnownNat bb_sn
  => Counter a_n b_n a_m aa_sm bb_sn
  -> Counter a_n b_n a_m aa_sm bb_sn
succWrap ctr@(a,b,c,d,e)
  | ctr == maxBound = minBound
  | a == maxBound && b == maxBound && c == maxBound && d == maxBound = (0,0,0,0,e+1)
  | a == maxBound && b == maxBound && c == maxBound = (0,0,0,d+1,e)
  | a == maxBound && b == maxBound = (0,0,c+1,d,e)
  | a == maxBound = (0,b+1,c,d,e)
  | otherwise = (a+1,b,c,d,e)

--------------------------------------------------------------------------------

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
      readerOutput :: Signal System (Maybe (Vec aa_sn Int, Vec bb_sm Int))
      readerOutput = mealy mmmult2dreader (Nothing, minBound) (fmap splitAB <$> ab)

      dotfOutput :: Signal System (Maybe Int)
      dotfOutput = register Nothing $ toSignal $ dotfm $ fromSignal readerOutput

  in fmap unsplitMatrix <$> mealy mmmult2dwriter (nullMatrix, minBound) dotfOutput

  where

    splitAB
      :: (Matrix a_m a_n Int, Matrix b_m b_n Int)
      -> (Matrix aa_m aa_n (Matrix aa_sm aa_sn Int), Matrix bb_m bb_n (Matrix bb_sm bb_sn Int))
    splitAB (a, b) =
      ( splitMatrix (SNat @aa_sm) aa_sn a
      , splitMatrix (SNat @bb_sm) (SNat @bb_sn) b
      )

    mmmult2dreader (Nothing, _) Nothing = ((Nothing, minBound), Nothing)
    mmmult2dreader _ matrices@(Just _) = ((matrices, minBound), Nothing)
    mmmult2dreader (matrices@(Just (matrixAA, matrixBB)), counter) _ = (newState, Just (rowA, colB))
      where

        newState
          :: ( Maybe (Matrix aa_m aa_n (Matrix aa_sm aa_sn Int), Matrix bb_m bb_n (Matrix bb_sm bb_sn Int))
             , Counter a_n b_n a_m aa_sm bb_sn
             )
        newState
          | counter == maxBound = (Nothing, succWrap counter)
          | otherwise = (matrices, succWrap counter)

        (aColI, _, aRowI, saRowI, _) = counter
        (bRowI, bColI, _, _, sbColI) = counter

        subA :: Matrix aa_sm aa_sn Int
        subA = matrixAA !! aRowI !! aColI

        subB :: Matrix bb_sm bb_sn Int
        subB = matrixBB !! bRowI !! bColI

        rowA = subA !! saRowI
        colB = transpose subB !! sbColI

    dotfm
      :: KnownNat n
      => KnownNat d
      => Num a
      => NFDataX a
      => HiddenClockResetEnable dom
      => DSignal dom d (Maybe (Vec n a, Vec n a))
      -> DSignal dom (d + n) (Maybe a)
    dotfm xy =
      foldrp
        -- Function:
        multAdd
        -- Defaults for output/input of function:
        Nothing
        undefined
        -- Vector to fold over:
        (DBundle.unbundle  $ uncurry zip . fromJust <$> xy)
        -- Start value; Nothing on no input, Just 0 on input:
        ((const 0 <$>) <$> xy)

      where
        multAdd _      Nothing  = Nothing
        multAdd (a, b) (Just c) = Just $ a * b + c

    nullMatrix :: Matrix aa_m bb_n (Matrix aa_sm bb_sn Int)
    nullMatrix = repeat $ repeat $ repeat $ repeat 0

    mmmult2dwriter _ Nothing = ((nullMatrix, minBound), Nothing)
    mmmult2dwriter (matrixRR, counter) (Just dotfResult) = (state', output)
      where

        state' ::
          ( Matrix aa_m bb_n (Matrix aa_sm bb_sn Int)
          , Counter aa_n bb_n aa_m aa_sm bb_sn
          )
        state' = (matrixRR'', succWrap counter)

        (matrixRR'', output)
          | counter == maxBound = (nullMatrix, Just matrixRR')
          | otherwise           = (matrixRR', Nothing)

        -- Calculate new partial result, store it in matrix R
        (_, rColI, rRowI, srRowI, srColI) = counter

        subR      = (matrixRR !! rRowI) !! rColI
        subR'     = alterMatrixElement subR srRowI srColI (+dotfResult)
        matrixRR' = replaceMatrixElement matrixRR rRowI rColI subR'


--------------------------------------------------------------------------------

foldrp
  :: forall dom a b d n.
     HiddenClockResetEnable dom
  => KnownNat n
  => NFDataX b
  => NFDataX a
  => KnownNat d
  => (a -> b -> b)
  -- ^ f
  -> b
  -- ^ default ouput
  -> a
  -- ^ default input
  -> Vec n (DSignal dom d a)
  -- ^ Vector to fold over
  -> DSignal dom d b
  -- ^ Start value
  -> DSignal dom (d + n) b
foldrp _ bStart _ Nil b = delayedI @n bStart b
foldrp f bStart  aStart (Cons a as) b =
  let b' = delayedI @1 bStart (f <$> a <*> b)
  in foldrp f bStart aStart (delayedI @1 aStart <$> as) b'

alterMatrixElement
  :: forall m n a.
     KnownNat m
  => KnownNat n
  => Matrix m n a
  -> Index m
  -> Index n
  -> (a -> a)
  -> Matrix m n a
alterMatrixElement a rowIndex colIndex f =
  let _sub = f $ a !! rowIndex !! colIndex
      sub' = replace colIndex _sub (a !! rowIndex)
  in replace rowIndex sub' a
