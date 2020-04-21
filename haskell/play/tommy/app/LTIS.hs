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

simplifySignal :: (Num a, Eq a, Floating a, Ord a) => Signal a -> Signal a
simplifySignal (Scale k (Sin a f s)) = Sin (a * k) f s
simplifySignal (Deriv   (Sin a f s)) = Sin (a * f) (-f) (pi/2 - s)
simplifySignal (Integ   (Sin a f s)) = Sin (-a/ f) (-f) (pi/2 - s)
simplifySignal (Sum (Sin a1 f1 s1)
                    (Sin a2 f2 s2)) | f1 == f2 = Sin a f1 s where
                      a = sqrt $ q1 ^ 2 + q2 ^ 2
                      s = atan $ q2 / q1
                      q1 = a1 * cos s1 + a2 * cos s2
                      q2 = a1 * sin s1 + a2 * sin s2
{-simplifySignal (Scale k other)
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
    o2' = simplifySignal o2-}
simplifySignal other = let ss = simplifySignal in case other of
  Scale k (Sum o1 o2)              -> Sum (ss $ Scale k o1) (ss $ Scale k o2)
  Deriv   (Sum o1 o2)              -> Sum (ss $ Deriv   o1) (ss $ Deriv   o2)
  Integ   (Sum o1 o2)              -> Sum (ss $ Integ   o1) (ss $ Integ   o2)
  Scale k a                        -> ss $ Scale k $ ss a
  Deriv   a                        -> ss $ Deriv   $ ss a
  Integ   a                        -> ss $ Integ   $ ss a
  Sum a b | ss a /= a || ss b /= b -> ss $ Sum (ss a) (ss b)
  Sum a@(Sum a1 a2) b@(Sum _ _)    -> ss $ Sum (ss $ Sum a1 b) (ss a2)
  Sum a b | maxfreq a < maxfreq b  -> ss $ Sum (ss b) (ss a)
  Sum (Sum c a@(Sin _ af _)) b@(Sin _ bf _)
                        | af == bf -> ss $ Sum (ss c) (ss $ Sum a b)
  _                                -> other

maxfreq :: (Ord a, Num a) => Signal a -> a
maxfreq (Sin _ f _) = f
maxfreq (Scale _ a) = maxfreq a
maxfreq (Deriv   a) = maxfreq a
maxfreq (Integ   a) = maxfreq a
maxfreq (Sum a b) | af < bf   = bf
                  | otherwise = af where
                    af = maxfreq a
                    bf = maxfreq b

evalSignal :: (Num a, Floating a, Eq a, Ord a) => Signal a -> (a -> a)
evalSignal     (Sin a f s) t = a * sin (f * t + s)
evalSignal     (Sum   a b) t = evalSignal a t + evalSignal b t
evalSignal     (Scale a b) t = a * evalSignal b t -- or evalSignal (simplifySignal sig) t
evalSignal sig@(Deriv a)   t = evalSignal (simplifySignal sig) t
evalSignal sig@(Integ a)   t = evalSignal (simplifySignal sig) t
