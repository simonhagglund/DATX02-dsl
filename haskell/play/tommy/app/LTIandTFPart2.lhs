\begin{code}
{-# LANGUAGE GADTs #-}
{-# Language StandaloneDeriving #-}
import Nyquist (C(C),R,re,im)
import Test.QuickCheck
\end{code}










\subsection{Combining Systems}\label{sec:combsys}

We previously talked about combining transfer functions of LTI systems (see \ref{sec:comb}). Let's now abstract away any details of transfer functions and focus on them as atomic blocks.
In the code below, we represent a transfer function with a string name (like \cmd{TF "G"}).
\begin{code}
data LTI where
  TF :: String -> LTI
\end{code}
There are a few ways of combining systems. First, recall (see \ref{sec:tftypes}) how systems can be simply fed into each other. As a nod to the block diagrams, we now label this with an arrow:
\begin{code}
  (:->) :: LTI -> LTI -> LTI
\end{code}
A signal may also be fed into two (or more) other systems:
\begin{code}
  (:-<) :: LTI -> (LTI, LTI) -> LTI
\end{code}
Lastly, a signal may be combined, either by adding or subtracting a signal. To simplify matters, we will only consider adding signals, since a signal could be negated with a transfer function.
\begin{code}
  (:>-) :: (LTI, LTI) -> LTI -> LTI
\end{code}
On the semantic end, recall how connecting two control systems together will multiply their transfer functions. The function \cmd{evalLTI} below takes a dictionary of transfer functions and evaluates the LTI data type to its transfer function.
\begin{code}
evalLTI :: (String -> (C -> C)) -> LTI -> (C -> C)
evalLTI dict (TF s) = dict s
evalLTI dict (a :-> b) = \z -> evalLTI dict a z * evalLTI dict b z
\end{code}
Splitting the signal in two direction will not change the signal, so we have:
\begin{code}
evalLTI dict (a :-< (b, c)) = undefined
evalLTI dict ((a, b) :>- c) = \z -> (evalLTI dict a z + evalLTI dict b z) * evalLTI dict c z
\end{code}

\todo[inline]{The point of this section is to make the point that all LTI systems' (or control systems' rather) TF are of the form P(s)/Q(s) where P and Q are sum(prod(TFs)).}

\end{newtext}








TESTING:
\begin{code}
simpleSys, feedback :: LTI
simpleSys = inp :-> tf :-> out where
  inp = TF "inp"
  out = TF "out"
  tf  = TF "G"


{-

  --→inp-→o--→[G]----→out--→
          ↑---[S]←-|

-}
feedback = inp :-> tf :-< (out, sup :-> feedback) where
  inp = TF "inp"
  out = TF "out"
  tf  = TF "G"
  sup = TF "S"


feedback2 = (inp, feedback2 :-> sup) :>- tf :-> out  where
  inp = TF "inp"
  out = TF "out"
  tf  = TF "G"
  sup = TF "S"


instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary

prop_Identity x = evalLTI dict (TF "G" :-> TF "H") x == ident x where
  dict "G" = ident
  dict "H" = ident
  ident x = 1

prop_Loop x = evalLTI2 100000 dict sys x ~= tf2x x * tfx2 x / (1 + tf2x x * tfx2 x) * tfid x where
  sys = (TF "U", sys :-> TF "neg") :>- TF "F" :-> TF "G"
  dict "F"   = tf2x
  dict "G"   = tfx2
  dict "neg" = tfneg
  dict "U"   = tfid
  tf2x  s = s * 2
  tfx2  s = s ^ 2
  tfneg s = -1
  tfid  s = 1


infixl 4 ~= -- same as ==
--(~=) :: (Ord a, Fractional a) => a -> a -> Bool
a ~= b = re (abs (a - b)) < 0.01

test x = evalLTI2 1000 dict sys x where
  sys = (TF "U", sys :-> TF "neg") :>- TF "F" :-> TF "G"
  dict "F"   = tf2x
  dict "G"   = tfx2
  dict "neg" = tfneg
  dict "U"   = tfid
  tf2x  s = s * 2
  tfx2  s = s ^ 2
  tfneg s = -1
  tfid  s = 1

evalLTI2 :: Int -> (String -> (C -> C)) -> LTI -> (C -> C)
evalLTI2 steps dict lti | steps == 0 = \z -> 1
                        | otherwise  = \z -> eval dict lti z where
  eval dict (a     :-> b) z = next a z * next b z
  eval dict ((a,b) :>- c) z = (next a z + next b z) * next c z
  eval dict (a :-< (b,c)) z = undefined
  eval dict (TF s)        z = dict s z
  next = evalLTI2 (steps - 1) dict


\end{code}
Tried testing LTI systems, above. Discovered that closed-loop systems cannot
be tested in this way since 1. they're infinite in depth, and 2. if evaluating
to a depth of n, let n->inf, generally, the transfer function will diverge due
to radius of convergence that emerges.



Second representation of (systems by) transfer functions:
\begin{code}

data TransFunc a where
  TransFunc :: String -> TransFunc a
  (:*:) :: TransFunc a -> TransFunc a -> TransFunc a
  (:/:) :: TransFunc a -> TransFunc a -> TransFunc a
  (:+:) :: TransFunc a -> TransFunc a -> TransFunc a
  (:-:) :: TransFunc a -> TransFunc a -> TransFunc a

toTFR :: LTI -> TransFunc a
toTFR (TF s)              = TransFunc s
toTFR (a      :-> b)      = toTFR a :*: toTFR b
toTFR ((a, b) :>- c)      = (toTFR a :+: toTFR b) :*: toTFR c
toTFR (a      :-< (b, c)) = undefined

-- equality on recursive instances
(===) :: LTI -> LTI -> LTI
(a1 :-> a2) === (b1 :-> b2) = undefined

deriving instance Eq LTI

type LTI2 = String -> String

--evalLTI2 :: (String -> (a -> a)) -> LTI2 -> String -> String -> (a -> a)
--evalLTI2 d lti inp out = lti out

testLTI2 :: String -> String
testLTI2 "out" = "G"
testLTI2 "G" = "inp"

\end{code}
Above DSL failed due to not adding more expressiveness than already existed
in the old system. Thought was to use equivalence checks that would work on
recursive definitions. Obviously this does not work all that well.
Addition 28/5: After discussion with Simon: if we assume a letter is used only
once in a system, any repetitions are due to recurison and branch can be
terminated when finding this repetition. To Be Evaluated/Continued...


New LTI DSL based on new mathematical calculations. :→: is regular ff; :<>: is
splitting a signal, then adding together again; Fb x y creates a feedback-loop
going around x, with y as block on fb-loop.
\begin{code}
data Lti where
  DDelt :: Lti
  Tf    :: String -> Lti
  (:→:) :: Lti -> Lti -> Lti
  (:<>:):: Lti -> (Lti, Lti) -> Lti
  Fb    :: Lti -> Lti -> Lti

testcl :: Lti
testcl = Fb (Tf "G" :→: Tf "H") DDelt

evalLti :: (Fractional a, Num a) => (String -> (a -> a)) -> Lti -> (a -> a)
evalLti d (Tf s) = d s
evalLti d (a :→: b) = evalLti d a * evalLti d b
evalLti d (a :<>: (b, c)) = evalLti d a * (evalLti d b + evalLti d c)
evalLti d (Fb a b) = evalLti d a / (1 - evalLti d a * evalLti d b)

-- TODO Migrate. Import funnuminst
instance Num b => Num (a -> b) where
  f + g = \x -> f x + g x
  f * g = \x -> f x * g x
instance Fractional b => Fractional (a -> b) where
  f / g = \x -> f x / g x

\end{code}
This proves easy to evaluate. Where the original was expressive enough to
be able to describe every system, this has some limiting factors. The DSL
could be expanded to provide full expressiveness, but this would make the
evaluation much less pretty.


To try out how well a fully expressive DSL would work:
\begin{code}
data LTi where
  Stop  :: LTi
  TrF   :: String -> LTi
  (:→)  :: LTi -> LTi -> LTi
  (:<>) :: LTi -> (LTi, LTi) -> LTi
  (:+>) :: LTi -> ((LTi -> LTi), LTi) -> LTi

syst = a :+> (\beta -> b :+> (\alpha -> c :→ beta :→ d :→ alpha :<> (e,e:→g) :→ h, f), unit) where
  a = TrF "A"
  b = TrF "B"
  c = TrF "C"
  d = TrF "D"
  e = TrF "E"
  f = TrF "F"
  g = TrF "G"
  h = TrF "H"
  unit = TrF "1"

evalLTi :: (Num a, Fractional a) => (String -> (a -> a)) -> LTi -> (a -> a)
evalLTi d l = fst (eval l) / (1 - (sum . snd . eval) l) where
  eval (Stop :→   _) = (1, [])
  eval (Stop :<>  _) = (1, [])
  eval (Stop :+>  _) = (1, [])
  eval (TrF s)         = (d s, [])
  eval (a :→  b)       = (eval a * eval b, [])
  eval (a :<> (b,  c)) = (eval a * (eval b + eval c), [])
  eval (a :+> (fb, f)) = (local, f * local) where
    local = eval (fb Stop)


prop_advLTITest = evalLTi d syst == undefined where
  d = undefined

\end{code}
