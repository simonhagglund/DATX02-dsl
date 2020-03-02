\begin{code}
{-# LANGUAGE GADTs #-}
module PartialFraction where
\end{code}

A partial fraction decomposition seeks to write a fraction with multiplied factors as a sequence of added fractions where the numerator has sameness.

To let us construct the a fraction that we can decompose, we need varaibles and numbers, where all numbers is \in \mathb{b}. And a two operators that binds Var and Const.

\begin{code}

data R = Double

data Expr where
    Var     :: Char -> Expr
    Const   :: Double -> Expr
    (:*:)   :: Expr -> Expr -> Expr
    (:+:)   :: Expr -> Expr -> Expr

type Fraction = (Expr, Expr)

partFacDecom :: Fraction -> [Fraction]
partFacDecom = undefined

\end{code}

Then we build fractions as

\begin{code}

ex1 :: Fraction
ex1 = (Const 1, (Var 's' :+: Var 'a'))

\end{code}

this fraction is a base case, i.e. if we apply partFacDecom on ex1 it returns the parameter.

\begin{code}

ex2LHS :: Fraction
ex2LHS = (Const 1, (Var 's' :+: Const 2) :*: (Var 's' :+: Const 2))

\end{code}

ex2 is not a base case and partFacDecom should return \frac{A}{s+1} + \frac{B}{s+1} or in our language.

\begin{code}
ex2RHS :: [Fraction]
ex2RHS =
    [ (Var 'A', (Var 's' :+: Const 2)))
    , (Var 'B', (Var 's' :+: Const 2) :*: (Var 's' :+: Const 2))
    ]
\end{code}

However ex2LHS = ex2RHS and both sides can be simplified by multipling both sides by the LHS numberator.
Such that 1 = A(s+2) + B.

What is left now is to choose A s such that A(s+2)=0, for which -2 is the solution. Hence B=1
and 1 = A(s+2) + 1 => A = 0.

So by substitution back A & B we arrive at the solutuion \frac{1}{(s+2)^2} = \frac{1}{(s+2)^2}

Much work for nothing..

