{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
import           Data.List (intersperse)

data Signal a where
  -- Amp -> Freq -> Time-shift -> (a -> a)
  Sin   :: Num a => a -> a -> a -> Signal a
  Sum   :: Signal a -> Signal a -> Signal a
  Scale :: a -> Signal a -> Signal a
  Deriv :: Signal a -> Signal a
  Integ :: Signal a -> Signal a

deriving instance Eq   a => Eq   (Signal a)
deriving instance Show a => Show (Signal a)

simplifySignal :: (Num a, Eq a, Floating a) => Signal a -> Signal a
simplifySignal (Scale k (Sin a f s)) = Sin (a * k) f s
simplifySignal (Deriv   (Sin a f s)) = Sin (a * f) (-f) (pi/2 - s)
simplifySignal (Integ   (Sin a f s)) = Sin (-a/ f) (-f) (pi/2 - s)
simplifySignal (Sum (Sin a1 f1 s1)
                    (Sin a2 f2 s2)) | f1 == f2 = Sin a f1 s where
                      a = sqrt $ q1 ^ 2 + q2 ^ 2
                      s = atan $ q2 / q1
                      q1 = a1 * cos s1 + a2 * cos s2
                      q2 = a1 * sin s1 + a2 * sin s2
simplifySignal (Scale k other)
  | other /= other' = simplifySignal $ Scale k other' where
             other' = simplifySignal other
simplifySignal (Deriv other)
  | other /= other' = simplifySignal $ Deriv   other' where
             other' = simplifySignal other
simplifySignal (Integ other)
  | other /= other' = simplifySignal $ Integ   other' where
             other' = simplifySignal other
simplifySignal (Sum o1 o2)
  | o1 /= o1' || o2 /= o2' = simplifySignal $ Sum o1' o2' where
    o1' = simplifySignal o1
    o2' = simplifySignal o2
simplifySignal other = other

evalSignal :: (Num a, Floating a, Eq a) => Signal a -> (a -> a)
evalSignal     (Sin a f s) t = a * sin (f * t + s)
evalSignal     (Sum   a b) t = evalSignal a t + evalSignal b t
evalSignal     (Scale a b) t = a * evalSignal b t -- or evalSignal (simplifySignal sig) t
evalSignal sig@(Deriv a)   t = evalSignal (simplifySignal sig) t
evalSignal sig@(Integ a)   t = evalSignal (simplifySignal sig) t
