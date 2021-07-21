module Example.Pipelined.Matrix where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as DBundle


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
