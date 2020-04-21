\documentclass{article}

\begin{document}

\section{Transfer Functions and LTI Systems}\label{sec:tf}
\todo[inline]{Subject to change}
\begin{code}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module TransferFunctions where
--  import Lib
\end{code}
\LNContinue
\begin{modtext}
\noindent
A transfer function is a function describing how a linear time-invariant (LTI) system (or part of such a system) transforms a time-varying input signal to give the system output.\iffalse (more on LTI systems in a bit; \ref{sec:lti}). \fi
A transfer function is stated in the (complex) frequency domain (i.e. after using the Laplace transform). Thus---from the naming convention from the Laplace transform---a function $f$ describing an LTI system has a corresponding transfer function labelled $F$.

In order to understand transfer functions well, we need to first gain some insight into LTI systems:
\end{modtext}

%This LTI-function is expressed only in $t$, and can therefore not be composed to express feeding a signal between different components. To make this more clear, let's take a look at the types of these functions.
\begin{newtext}
\subsection{Linear Time-Invariant Systems}\label{sec:ltisprimer}
A linear time-invariant (LTI) system is a system that is only indirectly dependent on time ($t$). More precisely, input signals may be linearly combined (superposed) or differentiated/integrated with respect to time.
%That an LTI system is time-invariant means that it is dependent on time ($t$), though, only indirectly so, with the use of derivatives.
As a consequence, an LTI system reacts exactly the same regardless of what value $t$ has, given identical inputs. (As a real-life parallel: the shower will change the water temperature in the same way when turning the valve, regardless of whether you do it in the morning or in the afternoon, today or tomorrow). We can express this property in code in the following way:
\transparent{0.5}
\begin{codeeq}
sys :: LTI; d :: Time; f :: Time -> Signal
-- Shifts a signal with some time d
shift :: Time -> (Time -> Signal) -> (Time -> Signal)
shift d f = \t -> f (t - d)
\end{codeeq}
\transparent{1}
\begin{codeeq}
sys (shift d f) == shift d (sys f)
\end{codeeq}
And we can express the property of linearity for LTI systems in code like this:
\transparent{.5}
\begin{codeeq}
a, b :: Double; f, g :: Time -> Signal
(+) :: (a -> b) -> (a -> b) -> (a -> b)
f + g = \x -> f x Prelude.+ g x
-- Scales the signal (amplitude) with
scale :: Double -> (Time -> Signal) -> (Time -> Signal)
scale k f = \t -> k * f t
\end{codeeq}
\transparent{1}
\begin{codeeq}
sys (scale a f + scale b g)
    == scale a (sys f) + scale b (sys g)
\end{codeeq}
%These systems are described using LTI-functions and transfer functions in an unusual way, which turns out to make the math easier, but tends to impede understanding. Let's take a closer look at the types involved:
These systems are described with transfer functions and what we call LTI-functions (i.e. the inverse Laplace transform of the transfer function), but in an unusual way, which turns out to make the math easier, but tends to impede understanding. Let's take a closer look at the types involved to understand this:
\end{newtext}

\subsection{Types}\label{sec:tftypes}
\begin{newtext}
Let's start with the LTI system itself. It takes an input signal ($u(t)$), transforms it, and yields an output signal ($y(t)$). The first thing to take notice of is what type the in- and output signals have:
\begin{align*}
    &Time = \mathbb{R}^{>0},~ Signal = \mathbb{R} \\
    &u, y : Time \rightarrow Signal
\end{align*}
The LTI system, $lti$, transforms its input signal in full. In other words, $lti$ is a second-order function---mapping a (single-parameter) function to another such function:
\begin{equation*}
    lti : (Time \rightarrow Signal) \rightarrow (Time \rightarrow Signal)
\end{equation*}
The LTI system needs the entirety of its input function---and not just the momentary value at $t$---in order to transform it into the output function.
It turns out that we can describe this transformation with another function of the same type as the in- and output functions. This is what we call the LTI-function, whose Laplace transformation is the system's transfer function.

Because of this roundabout way of describing this system, we cannot easily link two LTI systems together in an intuitive manner. Sure, we can define a system to be composed of two other LTI systems, like the following:
\begin{codeeq}
lti = lti1 . lti2
\end{codeeq}
This is works well since $lti$ is an endofunction (i.e. a function of type $a\rightarrow a$), but we do generally not have an expression for this transformation. What we do have, however, is the LTI-function (more on how it is defined in the next section; \ref{sec:lti}). The equivalent to composing LTI systems when working with LTI-functions (and in-/outputs to such systems since they have the same type) is convolution, which we saw (in \ref{sec:convol}) simplifies to regular multiplication when Laplace-transformed.
With an input function $u(t)$, an LTI system $\texttt{sys}$ with LTI-function $f(t)$, and an output function $y(t)$, we have the following relations:
\begin{codeeq}
lti u == y
U = laplace u; Y = laplace y; F = laplace f
Y s == \s -> U s * F s
y t == \t -> u t <*> f t
\end{codeeq}
\end{newtext}

\begin{modtext}
Consider the LTI system shown in the following block diagram:


%%%%%%%%%%%%%%%%%
%\missingfigure{Simple "feed-forward" system according to the following:\\ U(s)$\rightarrow$ G(s)$\rightarrow$ H(s)$\rightarrow$ Y(s)}
%\iffalse
\begin{figure}[H]
\centering
\begin{tikzpicture}[auto, node distance=2cm,>=latex']
    % Placing the blocks
    \node [input, name=input] {};
    \node [block, right of=input] (g) {G(s)};
    \node [block, right of=g,
            node distance=3cm] (h) {H(s)};
    % Arrows 1
    \draw [->] (g) -- node[name=u] {k} (h);
    \node [output, right of=h] (output) {};

    % Arrows 2
    \draw [draw,->] (input) -- node {u} (g);
    \draw [->] (h) -- node [name=y] {y}(output);
\end{tikzpicture}
\caption{\begin{modtext}A simple block diagram showing a subsystem with transfer function G(s) feeding into a subsystem with transfer function H(s). u(t) and y(t) are the in- and output signals, respectively. k(t) is the input of the H-system and output of the G-system.\end{modtext}}
\label{fig:tfex}
\end{figure}
%\fi
%%%%%%%%%%%%%%%%%
The functions U(s), G(s), K(s), H(s), and Y(s) are transfer functions derived from the LTI-functions u(t), g(t), k(t), h(t), and y(t).
The LTI-functions are functions of time that yield a signal strength, so we label their types:
\begin{align*}
    &Time = \mathbb{R}^{\geq 0},~ Signal = \mathbb{R} \\
    &u, g, k, h, y : Time \rightarrow Signal
\end{align*}
\begin{code}%
type Time   = Real
type Signal = Real
u, g, k, h, y :: Time -> Signal
\end{code}%
This determines the type of their transfer functions. Recall, $F(s)=\Lplc\left\{f\right\}(s)$ and the type of the Laplace transform is $\Lplc : (\mathbb{R}^{\geq 0}\rightarrow\mathbb{R})\rightarrow(\mathbb{C}\rightarrow\mathbb{C})$, making $F(s) : \mathbb{C}\rightarrow\mathbb{C}$. Thus,
\begin{equation*}
    U, G, K, H, Y : \mathbb{C} \rightarrow \mathbb{C}
\end{equation*}
\end{modtext}
\begin{newtext}
Note that even though we call all these functions transfer functions (and they are all of the same type), they do not all serve the same purpose. G(s) is the transfer function for the system represented by the left block in the block diagram and H(s) is the transfer function for the right one. U(s) is a transfer function representing the input signal. We can imagine this to be the transfer function of a system with $\delta(t)$ as input, feeding into the G-system (see figure \ref{fig:tfexext}). We here think of $\delta$ as a function that actuates the system. k(t) is an intermediate function sometimes written out to give a variable to multiple systems (this can be useful in more complex systems)---in this case $K(s)=U(s)\cdot G(s)$, i.e. the combined system of the input signal and the G-system. Y(s) is the transfer function for the full system \textit{and} U(s). The transfer function for the entire system is therefore $Y(s)/U(s)=G(s)\cdot H(s)$.
%%%%%%%%%%%%%%%%%
\begin{figure}[H]
\centering
\begin{tikzpicture}[auto, node distance=2cm,>=latex']
    % Placing the blocks
    \node [input, name=input] {};
    \node [block, right of=input] (u) {U(s)};
    \node [block, right of=u,
            node distance=3cm] (g) {G(s)};
    \node [block, right of=g,
            node distance=3cm] (h) {H(s)};
    % Arrows 1
    \draw [->] (u) -- node[name=k] {} (g);
    \draw [->] (g) -- node[name=k] {k} (h);
    \node [output, right of=h] (output) {};

    % Arrows 2
    \draw [draw,->] (input) -- node {$\delta$} (u);
    \draw [->] (h) -- node [name=y] {y}(output);
\end{tikzpicture}
\caption{\begin{newtext}Extended version of the block diagram shown in figure \ref{fig:tfex}. The input signal u(t) has been replaced by its transfer function with $\delta$ as input, acting as an actuator for the system. This is identical to the previous representation, but highlights how the input can be shown as a system with a transfer function U.\end{newtext}}
\label{fig:tfexext}
\end{figure}
%%%%%%%%%%%%%%%%%
Let's summarise this with some relations:
\transparent{.5}
\begin{codeeq}
type LTI = (Time -> Signal) -> (Time -> Signal)
sys :: LTI
f :: Time -> Signal; F :: C -> C
\end{codeeq}
\transparent{1}
\begin{codeeq}
sys = (<*> f)
\end{codeeq}
Or equivalently:
\begin{codeeq}
sys = \sig -> laplace sig * F
\end{codeeq}
where \texttt{sysf} has LTI-function f and transfer function F.
\end{newtext}

\subsection{Linear Time-Invariant Systems -- Part 2}\label{sec:lti}

\begin{modtext}
As mentioned above (in \ref{sec:ltisprimer}), a linear time-invariant (LTI) system is a system that is only indirectly dependent on time.\iffalse More precisely, input signals may be linearly combined (superposed) or differentiated/integrated with respect to time.
(As discussed (in \ref{sec:tf}), we can describe how an LTI system transforms an input signal to the output signal with a transfer function).\fi
%We can describe this in code the following way: %(recall that an LTI system has type $(T\rightarrow S)\rightarrow(T\rightarrow S)$)
An interesting property of this is that any sine function input into the system would still be a sine function, whereas any other type of wave function would change shape (not counting translation and scaling).
We let the following represent a signal processed by an LTI system:
\end{modtext}
\begin{code}
data Signal a where
  -- Amp -> Freq -> Time-shift -> (a -> a)
  Sin   :: Num a => a -> a -> a -> Signal a
  Sum   :: Signal a -> Signal a -> Signal a
  Scale :: a -> Signal a -> Signal a
  Deriv :: Signal a -> Signal a
  Integ :: Signal a -> Signal a
deriving instance Eq a => Eq (Signal a)
\end{code}
It should now be possible to take any Signal instance and simplify it to a single Sin constructor. Or, at least as long as frequencies match. But this is okay since an LTI system cannot change the frequency of a sine function.
\begin{code}
simplifySignal :: (Num a, Eq a, Floating a, Ord a) => Signal a -> Signal a
simplifySignal (Scale k (Sin a f s)) = Sin (a * k) f s
\end{code}
Recall how the derivative of $sin(t)$ is $cos(t)$, and that its antiderivative is $-cos(t)$, as well as the relationship $cos(t)=sin(\pi/2-t)$, in order to find the Sin constructor form of the following:
\begin{code}
simplifySignal (Deriv   (Sin a f s)) = Sin (a * f) (-f) (pi/2 - s)
simplifySignal (Integ   (Sin a f s)) = Sin (-a/ f) (-f) (pi/2 - s)
\end{code}
Summing two sine functions isn't quite as neat (no need to pay attention to the details here). And as mentioned, we disallow differing frequencies:
\begin{code}
simplifySignal (Sum (Sin a1 f1 s1)
                    (Sin a2 f2 s2)) | f1 == f2 = Sin a f1 s where
                      a = sqrt $ q1 ^ 2 + q2 ^ 2
                      s = atan $ q2 / q1
                      q1 = a1 * cos s1 + a2 * cos s2
                      q2 = a1 * sin s1 + a2 * sin s2
\end{code}
Since we now have this line which is not guaranteed to evaluate to a Sin constructor (because f1 might not equal f2) and in order to propagate the function \texttt{simplifySignal} over the syntax tree, we include the following to the function definition:
\iffalse
\begin{code}
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
\end{code}
\fi
\begin{code}
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
\end{code}
Apart from the caveat with frequency, we can quite easily\todo{We could explore this with laplace (or fourier would suffice) to cast perhaps better light onto this fact. Fourier of a sum of sines of equal freq would yield one peak.} see how it is that sine functions retain their shape in an LTI system. We could exclude frequency from the definition of the data type in order to make things easier, but that would at the same time make it less expressive.

Lastly, let's define the semantics of the data type:
\begin{code}
evalSignal :: (Num a, Floating a, Eq a, Ord a) => Signal a -> (a -> a)
evalSignal     (Sin a f s) t = a * sin (f * t + s)
evalSignal     (Sum   a b) t = evalSignal a t + evalSignal b t
evalSignal     (Scale a b) t = a * evalSignal b t
\end{code}
Since we cannot find a derivative only from \texttt{evalSignal a} and \texttt{t}, we will have to rely on \texttt{simplifySignal} to evaluate \texttt{Deriv} and \texttt{Integ}:
\begin{code}
evalSignal sig@(Deriv a)   t = evalSignal (simplifySignal sig) t
evalSignal sig@(Integ a)   t = evalSignal (simplifySignal sig) t
\end{code}



\subsection{Combining Transfer Functions}\label{sec:comb}
Since the LTI-functions are not endofunctions (i.e. are not functions \textit{on} a set), they cannot be composed. So how do we combine several transfer functions in a larger system? To understand that, we first have to understand exactly what a transfer function is describing:

\begin{modtext}
The LTI system is by definition time-invariant, so functions are not directly expressed in time, though, they vary with time. They are not expressed as a function of their input signal (but rather with their time-varying LTI-function), so how does it vary with input signal? The basic idea is that its LTI-function, $f(t)$, expresses the ``impulse response'' of the system, i.e. the output signal given an instantaneous impulse as input (the dirac delta function $\delta$). Any other input to the system has to be convoluted with the system function (defined in \ref{sec:convol}.
The LTI-function is defined this way because $\delta$ is the identity of the convolution operator---in other words:
\begin{codeeq}
(dirac <*>) == (id :: (Time -> Signal) -> (Time -> Signal))
\end{codeeq}
%Naturally, convolution with $\delta(t)$ is the (second order) identity function (on $\mathbb{R}^{>0}$).
Luckily, the Laplace transform of two convoluted LTI-functions is simply the product of their transfer functions.
\end{modtext}

\begin{codeeq}
\f -> laplace (\t -> diracdelta t <*> f t)
\f -> laplace (\t -> diracdelta t) * laplace (\t -> f t)
\f -> (\t -> step t) * (\t -> F t)
{- step = const 1 when t>=0 -}
\f -> \t -> F t
\end{codeeq}
\begin{newtext}
Because this ``impulse response'' is the same thing as what we've been calling ``LTI-function,'' this is what the function is usually referred to as. But this name suggests that it is merely a special case among input responses, when, in fact, it is essential for defining LTI systems. We will henceforth use the commonly accepted term ``impulse response,'' but do also think of the function as the function that specifies how an LTI system transforms its input.
\end{newtext}


\subsection{Finding Transfer Functions}
%\todo[inline]{This section may have to be rewritten a bit to match the changes above.}
\begin{modtext}
An LTI system is often described with a differential equation. In this section we will take a closer look at how to find a transfer function for an LTI system, given a differential equation.

We will use the following differential equation as a running example (as per usual, $u$ is the input signal and $y$ is the output signal):
\begin{equation*} % Övningstal 1.21c
9y(t)+6\dot{y}(t)+\ddot{y}(t)=3u(t)+2\dot{u}(t)
\end{equation*}
\end{modtext}

% Is this really a subset of LDEs? (Since we have both y and u)
We give the following representation for a linear differential equation with
constant coefficients:

\begin{code}
data LCDE a where
  LCDE :: ([a],[a]) -> LCDE a

lcde = LCDE ([9,6,1], [3,2]) :: Fractional a => LCDE a
\end{code} % Further explanation for list representation (if not provided elsewhere)
The type consists of two lists, representing each side of the equation. The
leftmost (first) digit in each list represents the coefficient of the lowest order derivative.
No constant term is included (since this would make both the math and the
syntax more complicated).

\begin{equation*}
\text{LCDE}([a_0,a_1,...,a_k],[b_0,b_1,...,b_l]) \iff \sum\limits^{k}_{n=0}{a_n y^{(n)}(t)} = \sum\limits^{l}_{n=0}{b_n u^{(n)}(t)}
\end{equation*}

We provide a definition of the Laplace transform (introduced in the previous section), specific to this set of equations. For the purposes of the demonstration of transfer functions an exact definition is not essential, but is provided below in code section \ref{code:tflaplace}.

In a similar manner to the above type we use a list definition, but a third
list is needed to represent additional constant--$s^n$-terms. %, and a fourth list representing values of $f^{(n)}$.
In order to calculate this last list's coefficients, values of $y^{(n)}(0)$ and $u^{(n)}(0)$ are needed (notice how \texttt{laplace} in code section \ref{code:tflaplace} requires these values). Specifically, $c_n$ below (equation \ref{eq:csnterms}) is a sum of $a_i y^{(v)}(0) - b_j u^{(w)}$.
(We are going to simplify this in a bit).

\begin{align*}\begin{split}%\label{eq:transformed}
\text{Transformed}([a_0,a_1,...,a_k],[b_0,b_1,...,b_l];[c_0,c_1,...,c_m]) &\iff\\\iff \sum\limits^{k}_{n=1}{a_n s^{n-1} Y(s)} = \sum\limits^{l}_{n=1}{b_n s^{n-1} U(s)} + C&,
\end{split}\end{align*}
\begin{equation}\label{eq:csnterms}
C = \sum\limits^{m}_{n=0}{c_n s^n}
\end{equation}

% Can we make this not be interpreted by ghci and still have it displayed as code?
\refstepcounter{codecount}
\begin{code}[label={code:tflaplace},caption={A data type representing the equation after applying the Laplace transform to its left and right hand sides; A function definition for the Laplace transform of an instance of LCDE. The function \texttt{zipWidthL} referenced works like \texttt{zipWith} but extends the shorter list with the first parameter such that the output has the same length as the longer of the two lists. See appendix \ref{sec:appcodezipwithl} for details.}]
data Transformed a where
  Transformed :: ([a],[a]) -> [a] -> Transformed a

--                  equation  f^i(0) g^j(0)   L{equation}
laplace :: Num a => LCDE a -> [a] -> [a] -> Transformed a
laplace (LCDE (lhs, rhs)) f0 g0 = transformed where
  transformed = Transformed (lhs, rhs) (zipWithL 0 (-) lc rc)
  lc = map sum [zipWith (*) f0 (drop n lhs) | n <- [1..length lhs-1]]
  rc = map sum [zipWith (*) g0 (drop n rhs) | n <- [1..length rhs-1]]
\end{code}%

Albeit a bit convoluted, we now have a representation of LCDE when it is laplace transformed.
Notice how LCDE (lhs, rhs) $\mapsto$ Transformed (lhs, rhs) \_. The hole is a polynomial in $s$, $p(s)$, that gets evaluated with values of $y^{(n)}(0)$ and $u^{(n)}(0)$. We make the assumption that all of these values are constant zero, $f^{(n)}(0)=0$, since we can assume $u(t)$ is zero $\forall t\leq 0$, and thus we can drop these two extra lists of values and re-define the data type as follows:

\begin{code}
data Transformed a where
  Transformed :: ([a],[a]) -> Transformed a deriving Eq

laplace :: Num a => LCDE a -> Transformed a
laplace (LCDE (lhs, rhs)) = Transformed (lhs, rhs)
\end{code}

The values don't change here---only the semantic information.
For our running example, lcde defined above, would evaluate to the following:
% Any better way to do this perhaps?
\begin{code}
lcdeTrans1 = laplace lcde
lcdeTrans2 = Transformed ([9,6,1], [3,2])
truth = lcdeTrans1 == lcdeTrans2
\end{code}

We expect our linear differential equation with constant coefficients to result
in a rational expression, so let's define a data type for rational expressions.

\begin{code}
data RatExpr a where
  RatExpr :: Num a => ([a],[a]) -> RatExpr a
\end{code}

We find the rational expression simply by solving for G(s)=Y(s)/U(s):
\begin{gather}
\sum\limits^{k}_{n=0}{a_n s^n Y(s)} = \sum\limits^{l}_{n=0}{b_n s^n U(s)}\\
Y(s)\sum\limits^{k}_{n=0}{a_n s^n} = U(s)\sum\limits^{l}_{n=0}{b_n s^n}\\
Y(s)/U(s) = \sum\limits^{l}_{n=0}{b_n s^n}/\sum\limits^{k}_{n=0}{a_n s^n}
\end{gather}

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

To make sense of this construction, we can make a simple eval-function:

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

We can use this for any arbitrary differential equation of the same form. Consider $3\dot{y}(t)+2y(t)=\ddot{u}(t)-u(t)$, and the following code snippet:

\begin{code}
de = LCDE ([2,3],[-1,0,1]) :: Fractional a => LCDE a
tf2 = evalRE $ findTF de
\end{code}

\ifoptionfinal{}{
\subsection{Step Response}
}
\todo[inline,color=other]{necessary?}


\LNReset

\end{document}
