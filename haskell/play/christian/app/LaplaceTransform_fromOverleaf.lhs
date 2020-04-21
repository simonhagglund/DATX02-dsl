\begin{code} 
module LaplaceTransform where 
-- import Expression
import ComplexNumbers 
import FunNumInst 

\end{code} 

\section{Laplace transform}
The Laplace transform is a tool that enables us to look at a function or equation from a different perspective. More specifically, it takes a function of time and transforms it into a function of frequency.
The Laplace transform is often used to solve differential equations, but more on that later. 

First, some conventions: it is common to call the Laplace transform of a function by the capital version of the original function, e.g. the Laplace transform of $f$ is $F$, not to be confused with the primitive function of $f$. Unless otherwise specified, from here on $F$ will signify the Laplace transform of $f$. Although we won't use it, some texts write $\widetilde{f}$ for the Laplace transform of $f$.  Note also that we use ``curly brackets'' (\{ and \}) around the function. Some texts use ordinary parentheses, but we will stick to curly brackets to avoid ambiguities. 

\subsection{Types} 
\todo[inline,color=other]{I've used lambda calculus, both to disambiguate a bit between functions and values, but also to try to bridge the gap between math and Haskell (lambda calculus $\leftrightarrow$ anonymous functions). I'm worried it'll be confusing for the students though, as they might not have seen lambda calculus. On suggestion, I wrote $\lambda t \mapsto$ instead of the usual $\lambda t . $, i.e. I exchanged the dot to a mapsto-arrow; again to make it more similar to Haskell.}
%PJ: I suggest you write just f instead of \t->f(t). Given your comment about braces we then get L{f}(s) in the LHS of the Laplace def. which I think looks quite OK. I think you should have a short section/subsection about notation for functions where you mention the common confusion with "the function f(x)" etc. (But this part may come earlier or later.)
The definition of the Laplace transform is  
\begin{equation*}
    \Lplc \left\{ \lambda t \mapsto f(t)\right\}(s) = \int_0^\infty e^{-st} f(t) \, \dd t. % ordinary mathematical 
    %\Lplc \left\{ \lambda t\mapsto f(t)\right\}(s) = \int_0^\infty \lambda t\mapsto (e^{-st} f(t)) \, \text{d} t. % lambda calculus
\end{equation*}
What are the types in the definition? An initial inspection allows us to identify
\begin{code}
t   :: Real 
f   :: Real -> Real 
exp :: Complex -> Complex 
\end{code}
%PJ: Note that "exp" looks different on line 3 than in the Laplace def.
and from DSLsofmath\todo[color=other]{Add citation, probably around page 60} we know that 
\begin{code}
integ :: Real -> Real -> (Real -> Real) -> Real -- definite integral 
\end{code}% better type for it? 
i.e. the integral takes two real arguments (the limits) and a function, returning a real number. 
Letting $s$ be a real number as well works fine, but in fact it's possible to be a bit more general and let it be a complex number. 

From this we can read that the Laplace transform should have the type
\begin{code}
laplace :: (Real -> Real) -> (Complex -> Complex) 
\end{code}
%PJ: Alse here it is not completely clear that "laplace" on line 1 is the same as the curly L used in math mode.
i.e. it's something that takes a function and transforms it into another function. 



\subsection{The most common rules and their usage} 

The definition of the Laplace transform is clunky to actually work with, so common practise is to have a table of common functions and their Laplace transforms, and use some rules to calculate the harder problems. 
We will utilize this tabular approach. For example, we might be asked to find the Laplace transform of 
\begin{equation*}
    f(t) = e^{\alpha t}.
\end{equation*}
If we look in table \ref{tab:laplacetrans}, we can see that the corresponding laplace transform is 
\begin{equation*}
    \Lplc \left\{\lambda t\mapsto e^{\alpha t}\right\} = \lambda s \mapsto \frac{1}{s-\alpha}.
\end{equation*}
%PJ: I see now why you used the lambda notation in the Laplace definition - to prepare for these cases. But I still think a separate short text about this would be better. 
%PJ: I strongly recommend to use some macro for the lambda expressions to make sure you can change it in all places if you decide to change the formatting. For example, the use of \mapsto is a bit unusal with lambda.
If we wanted to solve this using the definition, we would have to solve the integral 

\begin{equation*}
    F(s) = \int_0^\infty e^{-st} e^{-\alpha t} \, \dd t.
\end{equation*}
While doable, it's definitely easier to solve this using the tabular approach. 


\subsubsection{Superposition} 
Superposition is also called linearity; the Laplace transform is linear. This means that if you have a sum that you want to transform, you can just transform the terms separately and then sum them. Mathematically this is written (if $f$ and $g$ are two functions to be transformed, and $\alpha$ and $\beta$ are two real numbers):
\begin{equation*}
    \Lplc \left\{\alpha f + \beta g\right\} = \alpha \Lplc\{f\} + \beta \Lplc \{g\} 
\end{equation*}

In Haskell, this could look like

    %laplace (a * f + b * g) = \s -> (a * laplace f) s + (b * laplace g) s 
\begin{code}  
laplace (\t -> a * f t + b * g t ) = \s -> a * F s + b * G s 
                where F = laplace f s
                      G = laplace g s 
\end{code} 
%PJ: Type / scope error in def. of F and G: the variable s is never used on the RHS.
i.e. we create two separate functions: \verb|a * laplace f| and \verb|b * laplace g|, and then we sum them. 


\begin{example}
    Use the rule 
\begin{equation*}
    \Lplc \{\lambda t\mapsto e^{at}\} = \frac{1}{s-a}
\end{equation*}
    to Laplace transform 
\begin{equation*}
    f = \lambda t \mapsto (3e^{-t} + 5 e^{-3t})
\end{equation*}
or, written in Haskell-style
\begin{code} 
    --laplace (3 * exp (-t) + 5 * exp ((-3) * t)) 
\end{code} 
\end{example}
\begin{solution}

\end{solution} 

\subsubsection{Derivative} 
Sometimes we want to find the Laplace transform of the derivative of a function. This most often occurs when trying to solve differential equations. The typical way this is written in math texts is
%PJ: s unbound.
%PJ: I suggest L{f'}(s) on the LHS
\begin{equation*}
    \Lplc \left\{\lambda t \mapsto f'(t) \right\} = s \Lplc \left\{\lambda t \mapsto f(t) \right\} - f(0). 
\end{equation*}

If we assume that we have \verb|D|, a haskell implementation of derivative, e.g. \verb|D sin = cos|.
Then this rule can be written

\begin{code}
laplace (D f) s = s * (laplace f) - Const (f 0)
\end{code}

\iffalse 
\begin{align*} 
    \Lplc \{ f'(t) \}(s) &= s F(s) - f(0) \\ 
                         &= s F(s) - 1. 
\end{align*} 
Then we do the same thing to the right hand side. 

\begin{align*} 
    \Lplc \{- f(t)\} = - F(s).
\end{align*} 

Setting these two expressions equal gives us
\begin{align*} 
     s F(s) - 1 &= - F(s) \implies{} \\ 
     s F(s) + F(s) &= 1  \\
     (s+1) F(s) &= 1  \\
     F(s) &= \frac{1}{s+1}
\end{align*} 
\fi

\iffalse
One of the most important properties of the Laplace transform is that
\begin{equation*}
    \Lplc \left\{ f'(t)\right\} (s) = s F(s) - f(0).
\end{equation*}
This allows us to transform a differential equation into an algebraic equation, i.e. one that only includes the typical operations of algebra.

\fi 

\begin{example}
Use the Laplace transform to find f satisfying 
\begin{equation*}
    f'(t) = - f(t) \qquad f(0) = 1
\end{equation*} 

\end{example}
\begin{solution}
What we want to do is apply the Laplace transform to both sides of the equation, and then use the resulting equation to find an explicit expression for $F(s)$. 

\todo[inline,color=other]{I tried writing the calculations less math-y and writing it like it was code, kinda. I'm not sure if this is the way to go, but it might be better if we typeset it properly?}
First, we apply the Laplace transform to the left hand side, which gives us 
\begin{code} 
laplace f' s = s * laplace f s - f 0 
           --s = s * laplace f s - Const 1 
\end{code}

Then we do the same thing to the right hand side. 
\begin{code}
-- laplace (negate (f t)) = \s -> - (laplace f s) 
\end{code} 

if we let \verb|F = laplace f s| and write these two sides as equal to each other: 

\begin{codeeq} 
s * F s - Const 1 = - (F s)
\end{codeeq} 

if we move all instances of F s to one side and everything else to one side we get

\begin{codeeq}
s * F s + F s = Const 1.
\end{codeeq}
Factor out F s:

\begin{codeeq}
F s * (s + 1) = Const 1
\end{codeeq} 
and divide by (s + 1):

\begin{codeeq}
F s = Const 1 / (s + 1)
\end{codeeq} 
or in mathematical writing: 
\begin{equation*} 
    F(s) = \frac{1}{s+1}
\end{equation*} 


Now, if we look at the table of Laplace transforms, we can find that this is the transform of $e^{-t}$, so we can assume that the answer is $f(t) = e^{-t}$ (and this is in fact correct!).

Although we are done with the exercise now, a good habit to get into early is to always double check if the function satisfies the differential equation. If we differentiate $f(t)$ we get
\begin{equation*}
    f'(t) = -e^{-t} = - f(t).
\end{equation*}
Thus we know that one of the conditions holds. Inserting $t=0$ into $f(t)$ gives us 
\begin{equation*}
    f(0) = e^{-0} = 1,
\end{equation*}
so the second condition holds as well. Now we know that $f(t) = e^{-t}$ really is a solution to the differential equation. 

Checking if the function satisfied the equation might seem excessive, but when solving slightly more complicated problems one often have to use partial fraction decomposition, in which it's very easy to make mistakes. Checking if the solution matches the equation is a simple way to check if your result is correct. 

\end{solution}


\subsubsection{Integral} 
Now one might wonder: if there is a rule for derivatives, is there one for integrals? Luckily, such a rule exists! 

\begin{equation*}
    \Lplc \left\{ \lambda t\mapsto \int_0^t f(x)\, \dd x \right\}(s) = \frac{\Lplc \left\{\lambda t\mapsto f(t)\right\}(s)}{s}
\end{equation*}
Note that the integral 
\begin{equation*}
    \int_0^t f(x)\, \dd x = F(t) - F(0) 
\end{equation*}
(where $F$ is the primitive function of $f$, not the laplace transform), i.e. that this integral functions more like an indefinite integral than a definite one. 


It's interesting to note how the Laplace transform turns the derivatives and integrals, which are each others inverses, into multiplications and divisions, which also are each others inverses. 

If we assume that we have a haskell command 
\begin{code}
integ :: Real -> Real -> (Real -> Real) -> Real 
integ 0 t f = \t -> t 
-- the integral from a to b of the function f is written integ a b f
\end{code} 
which calculates the definite integral of a function. Then the rule can be written

\begin{code}
-- laplace (integ 0 t f) s = (laplace f) / s 
\end{code}
\subsubsection{Convolution} 
For combining two functions describing LTI systems together, we use convolution. (More on this later, in \ref{sec:comb}).
For two functions $f$ and $g$ defined on $[0,\infty)$, convolution is defined as
\begin{equation*}
    (f \Ast g)(t) = \int_0^t f(\tau) g(t-\tau) \, \dd\tau 
\end{equation*}
Normally convolution is denoted with an asterisk, $\ast$, but since this coincides with the Haskell symbol for multiplication we will use $\Ast$ instead.

The Laplace transform of convolution is 
\begin{equation*}
    \Lplc \left\{\lambda t \mapsto (f\Ast g)(t)\right\} = \Lplc\{\lambda t \mapsto f(t)\} \cdot \Lplc\{\lambda t \mapsto g(t)\}, 
\end{equation*}
i.e. the laplace transform turns convolution into multiplication. 

\begin{code}
-- laplace (\t -> integ 0 t (\tau-> (f tau) * (g (t-tau)))) = 
    --laplace f * laplace g 
\end{code} 



\subsubsection{Time shift} 
 % L (y(t-T)) = e-(sT) Y(s) 
 Another rule that often shows up handles time shift in a function, i.e. what does the function look like if you start it after $T$ time. Mathematically, this is written
 \begin{equation*}
     \Lplc \left\{\lambda t\mapsto f(t-T)\right\}(s) = e^{-sT} \Lplc \{\lambda t \mapsto f(t)\}.
 \end{equation*}
this could be written % in haskell? 
\begin{code}
-- laplace (t -> f (t-T)) = s -> exp ((-s)*T) * (laplace f)
\end{code} % todo: maybe add shift function, so this becomes laplace (shift t f) = ... 

\subsubsection{Exponential decay} 
 % L (e^(-at) y(t)) = Y(s+a) 

Models damping of a system. If $f(t)$ is a function that models a physical system, then $e^{-at}f(t)$ is the damped version of that function. The Laplace transform of a damped function is given by
 
\begin{equation*}
    \Lplc \left\{\lambda t \mapsto \left(e^{-\alpha t} f(t)\right)\right\}(s) = \Lplc\left\{\lambda t \mapsto f(t)\right\}(s+\alpha)
\end{equation*}
 
\begin{code}
-- laplace (\t -> exp ((-a)*t) * f t) = \s -> laplace f (s+a)
\end{code}


\subsection{Inverse Laplace transform}  

So far, whenever we've found an expression for the Laplace transform of a function, we've handwaved it by saying ``and we recognize that this other function transforms into this function, thus it should be the answer''. This gives the right answer (obviously, or else we wouldn't teach you that...), but it's not entirely rigorous (and mathematicians love rigor!)

\todo[inline,color=other]{Is it really worth it to include the definition? I don't think anyone will have any use for it tbh.}
The ``proper'' way to do it is by using what's (appropriately) called the inverse Laplace transform. Like the regular Laplace transform, it's defined using an integral that's even more clunky than the ordinary Laplace transform. The definition goes that inverse Laplace transform, $\Lplc^{-1}$ of a function $F$ is given by

\begin{equation*}
    \Lplc^{-1} \left\{\lambda s \mapsto F(s)\right\} = \frac{1}{2\pi} \lim_{T\to\infty} \int_{\gamma - iT}^{\gamma + iT} e^{st} F(s) \dd s 
\end{equation*}





\iffalse
% his is old, I'm not sure whether it's worth keeping/using? 
If we define 
\begin{code}
-- type TimeDomain      = Real
-- type FrequencyDomain = Complex
\end{code}
then we can define the type of the Laplace transform accordingly: 
\begin{code} 
-- laplace :: TimeDomain -> FrequencyDomain 
\end{code}







\todo[inline,color=other]{introduce what a laplace transform is in words.
show how we construct it in a dsl. (show code)
some examples (just showing that the code works)
some examples from the book. as many different ones as possible, if some examples doesn't work with our code explain why.
}
\fi
