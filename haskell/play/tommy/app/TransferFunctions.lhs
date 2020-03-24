\documentclass{article}

\begin{document}


\begin{code}
{-# LANGUAGE GADTs #-}

module TransferFunctions where
  import Lib
\end{code}

A transfer function is a function describing how an LTI system (or part of
such a system) changes an input signal to give the system output. A transfer
function is stated in the (complex) frequency domain (i.e. after using the
Laplace transform).

Consider the following differential equation:
\begin{equation} % Övningstal 1.21c
\ddot{y}(t)+6\dot{y}(t)+9y(t)=2\dot{u}(t)+3u(t)
\end{equation}

% Is this really a subset of LDEs? (Since we have both y and u)
We give the following representation for a linear differential equation with
constant coefficients:

\begin{code}
data LCDE a where
  LCDE :: Num a => ([a],[a]) -> LCDE a

lcde = LCDE ([0,9,6,1], [0,3,1])
\end{code} % Further explanation for list representation (if not provided elseware)
The type consists of a two lists, representing each side of the equation. The
leftmost (first) digit in each list represents the coefficient of the least
significant derivative. % Correct terminology? (significance)

\begin{equation}
\text{LCDE}([a_0,a_1,...,a_k],[b_0,b_1,...,b_l]) \iff \sum\limits^{k}_{n=0}{a_n y^{(n)}(t)} = \sum\limits^{l}_{n=0}{b_n u^{(n)}(t)}}
\end{equation}

We provide some definition of the Laplace transform (introduced in the
previous section). For the purposes of the demonstration of transfer functions
an exact definition is not essential, but is provided below in [code]. \todo{add reference to code below}

In a similar manner to the above type we use a list definition, but a third
list is needed to represent additional constant-$s^n$-terms, and a fourth list
representing values of $f^{(n)}$.

\begin{align}\begin{split}
\text{Transformed}([a_0,a_1,...,a_k],[b_0,b_1,...,b_l],[c_0,c_1,...,c_m];[k_0,k_1,...,k_l]) &\iff\\\iff \sum\limits^{}_{n=0}{a_n s^n F(s)} = \sum\limits^{}_{n=0}{b_n s^n F(s)} + C&,
\end{split}\end{align}
\begin{equation}
C = \sum\limits^{}_{n=0}{c_n s^n f(0)}
\end{equation}

\begin{code}
data Transformed a where
  Transformed :: Num a => ([a],[a],[a]) -> [a] -> Transformed a

laplace :: Num a => LCDE a -> [a] -> Transformed a
laplace (LCDE (lhs, rhs)) ic = transformed where
  transformed = Transformed (lhs, rhs, zipWith (-) lc rc)
  lc = [zipWith (*) ic (drop n $ reverse (tail lhs)) | n <- [0..len lhs-2]] % WIP. Probably doesn't work yet. Haven't had the time to check it
  rc = [zipWith (*) ic (drop n $ reverse (tail rhs)) | n <- [0..len rhs-2]]
\end{code}

Albeit a bit convoluted, we now have a representation of LCDE when it is laplace transformed.
Notice how LCDE (lhs, rhs) $\mapsto$ Transformed (lhs, rhs, \_) \_. The first hole
is a polynomial in $s$, $p(s)$, that gets evaluated with values in the second hole.

lcde defined above, would evaluate to the following:
\begin{code}
lcdeTrans1 = laplace lcde
lcdeTrans2 =
truth = lcdeTrans1 == lcdeTrans2
\end{code}


\end{document}
