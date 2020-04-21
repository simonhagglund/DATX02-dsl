\documentclass{article}

\begin{document}


\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module TransferFunctions where
--import Lib
\end{code}

A transfer function is a function describing how an LTI system (or part of
such a system) changes an input signal to give the system output. A transfer
function is stated in the (complex) frequency domain (i.e. after using the
Laplace transform).

We will use the following differential equation as a running example:
\begin{equation} % Övningstal 1.21c
\ddot{y}(t)+6\dot{y}(t)+9y(t)=2\dot{u}(t)+3u(t)
\end{equation}

% Is this really a subset of LDEs? (Since we have both y and u)
We give the following representation for a linear differential equation with
constant coefficients:

\begin{code}
data LCDE a where
  LCDE :: Num a => ([a],[a]) -> LCDE a

lcde = LCDE ([9,6,1], [3,2]) :: Fractional a => LCDE a
\end{code} % Further explanation for list representation (if not provided elseware)
The type consists of a two lists, representing each side of the equation. The
leftmost (first) digit in each list represents the coefficient of the least
significant derivative. % Correct terminology? (significance)
No constant term is included (since this would make both the math and the
syntax more complicated).

\begin{equation}
\text{LCDE}([a_0,a_1,...,a_k],[b_0,b_1,...,b_l]) \iff \sum\limits^{k}_{n=0}{a_n y^{(n)}(t)} = \sum\limits^{l}_{n=0}{b_n u^{(n)}(t)}
\end{equation}

We provide some definition of the Laplace transform (introduced in the
previous section). For the purposes of the demonstration of transfer functions
an exact definition is not essential, but is provided below in [code]. \todo{add reference to code below}

In a similar manner to the above type we use a list definition, but a third
list is needed to represent additional constant-$s^n$-terms, and a fourth list
representing values of $f^{(n)}$. (We are going to simplify this in a bit).

\begin{align}\begin{split}
\text{Transformed}([a_0,a_1,...,a_k],[b_0,b_1,...,b_l],[c_0,c_1,...,c_m];[k_0,k_1,...,k_l]) &\iff\\\iff \sum\limits^{}_{n=0}{a_n s^n F(s)} = \sum\limits^{}_{n=0}{b_n s^n F(s)} + C&,
\end{split}\end{align}
\begin{equation}
C = \sum\limits^{}_{n=0}{c_n s^n f(0)}
\end{equation}

% Can we make this not be interpreted by ghci and still have it displayed as code?
%\begin{code}
data Transformed a where
  Transformed :: Num a => ([a],[a]) -> [a] -> Transformed a

--                  equation  f^i(0) g^j(0)   L{equation}
laplace :: Num a => LCDE a -> [a] -> [a] -> Transformed a
laplace (LCDE (lhs, rhs)) f0 g0 = transformed where
  transformed = Transformed (lhs, rhs) (zipWithL 0 (-) lc rc)
  lc = map sum [zipWith (*) f0 (drop n lhs) | n <- [1..length lhs-1]]
  rc = map sum [zipWith (*) g0 (drop n rhs) | n <- [1..length rhs-1]]
%\end{code}

Albeit a bit convoluted, we now have a representation of LCDE when it is laplace transformed.
Notice how LCDE (lhs, rhs) $\mapsto$ Transformed (lhs, rhs, \_) \_. The first hole
is a polynomial in $s$, $p(s)$, that gets evaluated with values of $f^{(n)(0)}$
given by the second hole. We make the assumption that all of these values are constant
zero, $f^{(n)(0)}=0$, since we can assume u(t) is zero $\forall t\leq 0$, and thus
we can drop these two extra lists of values and re-define the data type as follows:

\begin{code}
data Transformed a where
  Transformed :: Num a => ([a],[a]) -> Transformed a

instance Eq a => Eq (Transformed a) where
  Transformed (alhs,arhs) == Transformed (blhs,brhs) = alhs == blhs && arhs == brhs

laplace :: Num a => LCDE a -> Transformed a
laplace (LCDE (lhs, rhs)) = Transformed (lhs, rhs)
\end{code}

The values don't change here---only the semantic information.
For our running example, lcde defined above, would evaluate to the following:
% Any better way to do this perhaps?
\begin{code}
lcdeTrans1 = laplace lcde
lcdeTrans2 = Transformed ([9,6,1], [3,1])
truth = lcdeTrans1 == lcdeTrans2
\end{code}

We expect our linear differential equation with constant coefficients to result
in a rational expression, so let's define a data type for rational expressions.

\begin{code}
data RatExpr a where
  RatExpr :: Num a => ([a],[a]) -> RatExpr a
\end{code}

We find the rational expression simply by solving for G(s)=Y(s)/U(s):
\begin{equation}
\sum\limits^{k}_{n=0}{a_n s^n Y(s)} = \sum\limits^{l}_{n=0}{b_n s^n U(s)}

Y(s)\sum\limits^{k}_{n=0}{a_n s^n} = U(s)\sum\limits^{l}_{n=0}{b_n s^n}

Y(s)/U(s) = \sum\limits^{l}_{n=0}{b_n s^n}/\sum\limits^{k}_{n=0}{a_n s^n}
\end{equation}

\begin{code}
solve2RatExpr :: Num a => Transformed a -> RatExpr a
solve2RatExpr (Transformed (lhs, rhs)) = RatExpr (lhs, rhs)
\end{code}

Again, this is in the end just a change of constructor name, i.e. changing the % Is this some morphism something-or-other since structure doesn't change?
semantics. Combining the two we see nothing new:

\begin{code}
findTF :: Num a => LCDE a -> RatExpr a
findTF = solve2RatExpr.laplace
\end{code}

To make sense of this we can make a simple eval-function:

\begin{code}
evalRE :: Fractional a => RatExpr a -> a -> a
evalRE (RatExpr (den,num)) s = numerator / denominator where
  numerator   = unravel num
  denominator = unravel den
  unravel []   = 0
  unravel list = head list + s * unravel (tail list)

tfG s = evalRE (findTF lcde) s
\end{code}

The function tfG above is the transfer function from the differential equation lcde.




\begin{code}
-- HELPER FUNCTIONS --
--       null-val zip-function   left   right  zipped
zipWithL :: a -> (a -> a -> b) -> [a] -> [a] -> [b]
zipWithL i f (l:left) (r:right) = f l r : zipWithL i f left right
zipWithL _ _ []       []        = []
zipWithL i f []       (r:right) = f i r : zipWithL i f []   right
zipWithL i f (l:left) []        = f l i : zipWithL i f left []
\end{code}

\end{document}
