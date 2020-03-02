\begin{code}
{-# LANGUAGE GADTs #-}
module Complex where
\end{code}

The the surjective function \arg is a function that given a complex number always returns a real number. It is surjective since it collapses the parameter of the function to a angle on the intervall 2 \pi > a > 0. Where four cases is needed.

\begin{code}

type R = Double

type Complex = (R, R)

\end{code}

A decleration for arg is as thus as follows
\begin{code}
arg :: Complex -> R
\end{code}

and a implementation where r > 0 is written as.

\begin{code}
arg (r, i) | r > 0 = atan (i/r)
\end{code}

since r is greater then zero, we can deduce that this number lies in the RH were and the division of r with i yields the ratio of the segments 0 -> r in R  and r -> i in C, hence arctan yields the angle.

\begin{code}
arg (r, i) | i > 0 = (pi/2) - atan (i/r)
\end{code}

since i is greater then zero the angle lies in upper moste quadrantes, such that if we start in (-1, 0) and move backwards with \tan^{-1} \frac{i}{r} the angle is found.

\begin{code}
arg (r, i) | i < 0 = - (pi/2) - atan (i/r)
\end{code}

the angle lies in in the bottom two quadrantes. Since i is neative the \tan^{-1} \frac{i}{r} will generate a positive number such that -\frac{\pi}{2} - (-a).

\begin{code}
arg (r, i) | i < 0 = atan (i/r) + (pi/2)
\end{code}

%% MEH.
