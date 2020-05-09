\begin{code}
module Nyquist where



type R = Double
data C = C R R

re, im :: C -> R
re (C r _) = r
im (C _ i) = i

instance Num C where
  C ar ai + C br bi = C (ar + br) (ai + bi)
  C ar ai - C br bi = C (ar - br) (ai - bi)
  C ar ai * C br bi = C (ar * br - ai * bi) (ar * bi + ai * br)
  negate   (C r i)  = C (negate r) (negate i)
  abs      (C r i)  = C (sqrt $ r * r + i * i) 0
  signum z@(C r i)  = C (r / (re.abs) z) (i / (re.abs) z)
  fromInteger i     = C (fromInteger i) 0

instance Show C where
  show (C r i) | i == 0 = show r
               | r == 0 = show i ++ "i"
               | otherwise = show r ++ sign ++ show i ++ "i" where
                 sign | i < 0 = ""
                      | otherwise = "+"

(*&) :: R -> C -> C
c *& C r i = C (c * r) (c * i)
infixl 7 *& -- same as *

contourAng :: R -> [C] -> [C]
contourAng dist points = concatMap (subdiv dist) $ pairs ps where--concatMap subdiv $ pairs ps where
  ps = points ++ [head points]
  pairs (a:b:cs) = (a,b) : pairs (b:cs)
  pairs [_]      = []

subdiv :: R -> (C, C) -> [C]
subdiv d (a, b) = [ a + n *& seg | n <- [1..nseg]] where
  len = re.abs $ b - a
  nseg = fromIntegral.floor $ len / d
  seg = (d / len) *& (b - a)

contourSemi :: R -> [C]
contourSemi d = subdiv d (C 0 0, inf) ++ arc ++ subdiv d (-inf, C 0 0) where
  inf = C 0 (1 / d)
  arc = [inf * (sec ^ k) | k <- [1..narcseg]]
  narcseg = floor $ arclength / d
  arclength = pi * im inf
  sec = C (cos (d^2)) (-sin (d^2)) -- ^2 instead of /inf

\end{code}

mapping contours:

\begin{code}

-- Transfer Function
tf :: C -> C
tf = undefined

-- Limit to 0+
limit :: Num a => (a -> b) -> b
limit = undefined

cf = limit contourSemi

mapped = map tf cf

\end{code}
