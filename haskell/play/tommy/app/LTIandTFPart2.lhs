\begin{code}
{-# LANGUAGE GADTs #-}
import Nyquist (C,R)
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
\begin{code}
  Out :: LTI -> LTI
  Inp :: LTI -> LTI
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
\end{code}
