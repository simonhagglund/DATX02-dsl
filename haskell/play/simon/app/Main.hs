{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib

main :: IO ()
main = someFunc

data Expr s where -- Syntax of expression DSL.
    Id          :: Expr a            -- Identity, i.e x in math (f(x) = x)
    Const       :: a -> Expr a       -- Constant function f(x) = 1
    (:+:)       :: Expr s -> Expr s -> Expr s   -- Addition : x+y
    (:-:)       :: Expr s -> Expr s -> Expr s   -- Subtraction : x-y
    (:*:)       :: Expr s -> Expr s -> Expr s   -- Multiplication : x*y
    (:/:)       :: Expr s -> Expr s -> Expr s   -- Division : x/y
    (:**:)      :: Expr s -> Expr s -> Expr s   -- Exponent : x^y
    LogBase     :: Expr s -> Expr s -> Expr s   -- Log to what base: Log_x y
    Neg         :: Expr s -> Expr s             -- Negate: >1
    Abs         :: Expr s -> Expr s             -- Absolute value |x|
    Signum      :: Expr s -> Expr s             -- Significand: x>0 = 1, x==0 = 0, x<0 = -1
    Exp         :: Expr s -> Expr s             -- f(x) = e^x
    Sqrt        :: Expr s -> Expr s             -- square root of.
    Log         :: Expr s -> Expr s             -- f(x) = log(x)
    Sin         :: Expr s -> Expr s             -- f(x) = sin(x)
    Cos         :: Expr s -> Expr s             -- f(x) = cos(x)
    Tan         :: Expr s -> Expr s             -- f(x) = tan(x)

showExprTD :: ExprTD -> String
showExprTD Id                             = "x"
showExprTD (Const c)                      = show c
-- Addtion
showExprTD ((Id)      :+: (Id))           = "x+x"
showExprTD ((Const c) :+: (Const c'))     = show c ++ "+" ++ show c'
showExprTD ((Id)      :+: (Const c))      = "x + " ++ show c
showExprTD ((Const c) :+: (Id))           = show c ++ " + x"
showExprTD ((Const c) :+: e)              = show c ++ " + (" ++ show e ++ ")"
showExprTD (Id        :+: e)              = "x + (" ++ show e ++ ")"
showExprTD (e         :+: (Const c))      = "(" ++ show e ++ ") + " ++ show c
showExprTD (e         :+: Id)             = "(" ++ show e ++ ") + x"
showExprTD (e         :+: e')             = "(" ++ show e ++ ") + (" ++ show e' ++ ")"
-- Subtraction
showExprTD ((Id)      :-: (Id))           = "x - x"
showExprTD ((Const c) :-: (Const c'))     = show c ++ "-" ++ show c'
showExprTD ((Id)      :-: (Const c))      = "x - " ++ show c
showExprTD ((Const c) :-: (Id))           = show c ++ "+ -"
showExprTD ((Const c) :-: e)              = show c ++ " - (" ++ show e ++ ")"
showExprTD (Id        :-: e)              = "x - (" ++ show e ++ ")"
showExprTD (e         :-: (Const c))      = "(" ++ show e ++ ") - " ++ show c
showExprTD (e         :-: Id)             = "(" ++ show e ++ ") - x"
showExprTD (e         :-: e')             = "(" ++ show e ++ ") - (" ++ show e' ++ ")"
-- Multiplication
showExprTD ((Id)      :*: (Id))           = "x*x"
showExprTD ((Const c) :*: (Const c'))     = show c ++ "*" ++ show c'
showExprTD ((Id)      :*: (Const c))      = show c ++ "*x"
showExprTD ((Const c) :*: (Id))           = show c ++ "*x"
showExprTD ((Const c) :*: e)              = show c ++ "*(" ++ show e ++ ")"
showExprTD (Id        :*: e)              = "x*(" ++ show e ++ ")"
showExprTD (e         :*: (Const c))      = show c ++ "*(" ++ show e ++ ")"
showExprTD (e         :*: Id)             = "x*(" ++ show e ++ ")"
showExprTD (e         :*: e')             = "(" ++ show e ++ ")*(" ++ show e' ++ ")"
-- Division
showExprTD ((Id)      :/: (Id))           = "x/x"
showExprTD ((Const c) :/: (Const c'))     = show c ++ "/" ++ show c'
showExprTD ((Id)      :/: (Const c))      = "x/" ++ show c
showExprTD ((Const c) :/: (Id))           = show c ++ "/x"
showExprTD ((Const c) :/: e)              = show c ++ "/(" ++ show e ++ ")"
showExprTD (Id        :/: e)              = "x/(" ++ show e ++ ")"
showExprTD (e         :/: (Const c))      = "(" ++ show e ++ ")/" ++ show c
showExprTD (e         :/: Id)             = "(" ++ show e ++ ")/x"
showExprTD (e         :/: e')             = "(" ++ show e ++ ")/(" ++ show e' ++ ")"
-- Power of
showExprTD ((Id)      :**: (Id))          = "x^x"
showExprTD ((Const c) :**: (Const c'))    = show c ++ "^" ++ show c'
showExprTD ((Id)      :**: (Const c))     = "x^" ++ show c
showExprTD ((Const c) :**: (Id))          = show c ++ "^x"
showExprTD ((Const c) :**: e)             = show c ++ "^(" ++ show e ++ ")"
showExprTD (Id        :**: e)             = "x^(" ++ show e ++ ")"
showExprTD (e         :**: (Const c))     = "(" ++ show e ++ ")^" ++ show c
showExprTD (e         :**: Id)            = "(" ++ show e ++ ")^x"
showExprTD (e         :**: e')            = "(" ++ show e ++ ")^(" ++ show e' ++ ")"
-- Rest
showExprTD (LogBase e e')                 = "log_" ++ show e ++ "(" ++ show e' ++")"
showExprTD (Neg e)                        = "-(" ++ show e ++ ")"
showExprTD (Abs e)                        = "|" ++ show e ++ "|"
showExprTD (Exp e)                        = "e^(" ++ show e ++ ")"
showExprTD (Sqrt e)                       = "sqrt(" ++ show e ++ ")"
showExprTD (Log e)                        = "log(" ++ show e ++ ")"
showExprTD (Sin e)                        = "sin(" ++ show e ++ ")"
showExprTD (Cos e)                        = "cos(" ++ show e ++ ")"
showExprTD (Tan e)                        = "tan(" ++ show e ++ ")"
showExprTD (Signum e)                     = "sign(" ++ show e ++ ")"

-- | Naive printing of time domain expression.
instance Show ExprTD where
    show = showExprTD

idE :: Expr a
idE = Id

type Complex = Integer

type ExprTD = Expr Double   -- Expression in time domain.
type ExprFD = Expr Complex  -- Expression in frequency domain.

-- | Enables numerical mathematical notation like f(x) = g(x) + h(x)
instance Num ExprTD where
    (+)         = (:+:)
    (-)         = (:-:)
    (*)         = (:*:)
    negate      = Neg
    abs         = Abs
    signum      = Signum
    fromInteger = Const . fromIntegral

-- | Enables fractional mathematical notation like f(x) = g(x) / h(x)
instance Fractional ExprTD where
    (/) = (:/:)
    recip = (1/) -- gives 1 / argument
    fromRational = Const . fromRational

-- | Enables floating mathematical notation like f(x) = 2*pi g(x) ^ h(x) + sin (k(x))
instance Floating ExprTD where
  pi = Const pi
  exp = Exp
  log = Log
  sqrt = Sqrt
  (**) = (:**:)
  logBase = LogBase
  sin = Sin
  cos = Cos
  tan = Tan
  asin = undefined
  acos = undefined
  atan = undefined
  sinh = undefined
  cosh = undefined
  tanh = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined


-- | Like should probably use lookup tables like in the course.
transform :: ExprTD -> ExprFD
transform = undefined

-- | Partial fraction decomposition.
toPartial :: ExprFD -> Maybe ExprFD
toPartial = undefined

-- | Simplifies an expression.
simplify :: Expr s -> Expr s -- Simplify doubles is not intuitive.
simplify = undefined

-- | Semantics of DSL. Evaluates a expression, returning a new function.
evalExpr :: Floating t => Expr t -> t -> t
evalExpr Id             = id
evalExpr (Const c)      = const c
evalExpr (e :/: e')     = \s -> ((evalExpr e) s) / ((evalExpr e') s)
evalExpr (e :+: e')     = \s -> (evalExpr e) s + (evalExpr e') s
evalExpr (e :-: e')     = \s -> (evalExpr e) s - (evalExpr e') s
evalExpr (e :*: e')     = \s -> (evalExpr e) s * (evalExpr e') s
evalExpr (LogBase e e') = \s -> logBase (evalExpr e s) (evalExpr e' s)
evalExpr (Neg e)        = negate . evalExpr e
evalExpr (Abs e)        = abs . evalExpr e
evalExpr (Signum e)     = signum . evalExpr e
evalExpr (Exp e)        = exp . evalExpr e
evalExpr (Cos e)        = cos . evalExpr e
evalExpr (Sin e)        = sin . evalExpr e
evalExpr (Tan e)        = tan . evalExpr e
evalExpr (Sqrt e)       = sqrt . evalExpr e
evalExpr (Log e)        = log . evalExpr e

-- | f(t) = 2*t + (t +2)*3
example :: ExprTD
example = 2 * idE + (2 + idE)*3

-- | f(t) = 2*pi
example1 :: ExprTD
example1 = 2*pi / 4

test = evalExpr example $ 2 -- == 16

