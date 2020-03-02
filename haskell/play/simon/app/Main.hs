{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib

main :: IO ()
main = someFunc

-- | Didiktiska implimplikatiioner.
-- hitta balanser.
-- studie nämden,
-- ganska dåligt på nyttogörande aspekten. tenderas att glömas bort.


-- f(x) = 2 + x
-- f(x) = 2 + x

--f(x, const(2)) = const(2) + x

data Expr s where -- Syntax of expression DSL.
    Pow         :: Integer -> Expr s            -- Identity Pow 1, i.e x in math (f(x) = x)
    Const       :: s -> Expr s                  -- Constant function f(x) = 1
    (:+:)       :: Expr s -> Expr s -> Expr s    -- Addition : x+y
    (:*:)       :: Expr s -> Expr s -> Expr s   -- Multiplication : x*y
    (:-:)       :: Expr s -> Expr s -> Expr s   -- Subtraction : x-y
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
    D           :: Expr s -> Expr s             -- f'(x)
    Fun         :: String -> Expr s -> Expr s
        --deriving (Show)

-- Y(s) = U(s) * H(s)
-- hitta h
-- Y(s) / U(s) = H(s)

instance Show ExprTD where
    show = showExprTD

-- | Maybe multiplication can be a functor over expr.
instance Functor Expr where
    fmap f (Pow n)      = Pow n
    fmap f (Const c)    = Const (f c)
    fmap f (e :+: e')   = fmap f e :+: fmap f e'
    fmap f (e :*: e')   = fmap f e :*: fmap f e'
    fmap f (e :-: e')   = fmap f e :-: fmap f e'
    fmap f (e :/: e')   = fmap f e :/: fmap f e'
    fmap f e@(_ :**: _) = undefined -- Have nothing here.

instance Applicative Expr where
    pure = Const
    (Const f) <*> e = fmap f e


-- | Naive printing of time domain expression.
showExprTD :: ExprTD -> String
showExprTD (Pow 1)                        = "x"
showExprTD (Pow n)                        = "x^" ++ show n
showExprTD (Const c)                      = show c
-- Addtion
showExprTD ((Pow 1)   :+: (Pow 1))           = "x+x"
showExprTD ((Const c) :+: (Const c'))     = show c ++ "+" ++ show c'
showExprTD ((Pow 1)   :+: (Const c))      = "x + " ++ show c
showExprTD ((Const c) :+: (Pow 1))           = show c ++ " + x"
showExprTD ((Const c) :+: e)              = show c ++ " + (" ++ show e ++ ")"
showExprTD (Pow 1     :+: e)              = "x + (" ++ show e ++ ")"
showExprTD (e         :+: (Const c))      = "(" ++ show e ++ ") + " ++ show c
showExprTD (e         :+: Pow 1)             = "(" ++ show e ++ ") + x"
showExprTD (e         :+: e')             = "(" ++ show e ++ ") + (" ++ show e' ++ ")"
-- Subtraction
showExprTD ((Pow 1)   :-: (Pow 1))           = "x - x"
showExprTD ((Const c) :-: (Const c'))     = show c ++ "-" ++ show c'
showExprTD ((Pow 1)   :-: (Const c))      = "x - " ++ show c
showExprTD ((Const c) :-: (Pow 1))           = show c ++ "+ -"
showExprTD ((Const c) :-: e)              = show c ++ " - (" ++ show e ++ ")"
showExprTD (Pow 1     :-: e)              = "x - (" ++ show e ++ ")"
showExprTD (e         :-: (Const c))      = "(" ++ show e ++ ") - " ++ show c
showExprTD (e         :-: Pow 1)             = "(" ++ show e ++ ") - x"
showExprTD (e         :-: e')             = "(" ++ show e ++ ") - (" ++ show e' ++ ")"
-- Multiplication
showExprTD ((Pow 1)   :*: (Pow 1))           = "x*x"
showExprTD ((Const c) :*: (Const c'))     = show c ++ "*" ++ show c'
showExprTD ((Pow 1)   :*: (Const c))   = show c ++ "*x"
showExprTD ((Const c) :*: (Pow 1))        = show c ++ "*x"
showExprTD ((Const c) :*: (Pow n))        = undefined
showExprTD ((Const c) :*: e)              = show c ++ "*(" ++ show e ++ ")"
showExprTD ((Pow 1)   :*: e)              = "x*(" ++ show e ++ ")"
showExprTD ((Pow n)   :*: e)              = undefined
showExprTD (e         :*: (Const c))      = show c ++ "*(" ++ show e ++ ")"
showExprTD (e         :*: Pow 1)             = "x*(" ++ show e ++ ")"
showExprTD (e         :*: e')             = "(" ++ show e ++ ")*(" ++ show e' ++ ")"
-- Division
showExprTD ((Pow 1)   :/: (Pow 1))        = "x/x"
showExprTD ((Pow n)   :/: (Pow n'))       = undefined
showExprTD ((Const c) :/: (Const c'))     = show c ++ "/" ++ show c'
showExprTD ((Pow 1)   :/: (Const c))      = "x/" ++ show c
showExprTD ((Pow n)   :/: (Const c))      = undefined
showExprTD ((Const c) :/: (Pow 1))        = show c ++ "/x"
showExprTD ((Const c) :/: (Pow n))        = undefined
showExprTD ((Const c) :/: e)              = show c ++ "/(" ++ show e ++ ")"
showExprTD ((Pow 1)   :/: e)              = "x/(" ++ show e ++ ")"
showExprTD ((Pow n)   :/: e)              = undefined
showExprTD (e         :/: (Const c))      = "(" ++ show e ++ ")/" ++ show c
showExprTD (e         :/: (Pow 1))        = "(" ++ show e ++ ")/x"
showExprTD (e         :/: (Pow n))        = undefined
showExprTD (e         :/: e')             = "(" ++ show e ++ ")/(" ++ show e' ++ ")"
-- Power of
showExprTD ((Pow 1)   :**: (Pow 1))       = "x^x"
showExprTD ((Pow n)   :**: (Pow n'))      = undefined
showExprTD ((Const c) :**: (Const c'))    = show c ++ "^" ++ show c'
showExprTD ((Pow 1)   :**: (Const c))     = "x^" ++ show c
showExprTD ((Pow n)   :**: (Const c))     = "x^(" ++ show n ++ "(" ++ show c ++ ")"
showExprTD ((Const c) :**: (Pow 1))       = show c ++ "^x"
showExprTD ((Const c) :**: (Pow n))       = show c ++ "^x^" ++ show n
showExprTD ((Const c) :**: e)             = show c ++ "^(" ++ show e ++ ")"
showExprTD ((Pow 1)   :**: e)             = "x^(" ++ show e ++ ")"
showExprTD ((Pow n)   :**: e)             = "x^(" ++ show n ++ "(" ++ show e ++ "))"
showExprTD (e         :**: (Const c))     = "(" ++ show e ++ ")^" ++ show c
showExprTD (e         :**: (Pow 1))       = "(" ++ show e ++ ")^x"
showExprTD (e         :**: (Pow n))       = "(" ++ show e ++ ")^(x^" ++ show n ++ ")"
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

idE :: ExprTD
idE = Pow 1

type Complex = Integer

type ExprTD = Expr Double   -- Expression in time domain.
type ExprFD = Expr Complex  -- Expression in frequency domain.

-- | Enables numerical mathematical notation like f(x) = g(x) + h(x)
instance Num ExprTD where
    (+)             = (:+:)
    (-)             = (:-:)
    (*) (Pow n) e   = e :*: (Pow n)
    (*) e e'        = e :*: e'
    negate          = Neg
    abs             = Abs
    signum          = Signum
    fromInteger     = Const . fromIntegral

-- | Enables fractional mathematical notation like f(x) = g(x) / h(x)
instance Fractional ExprTD where
    (/) = (:/:)
    recip = (1/) -- gives 1 / argument
    fromRational = Const . fromRational

-- | Enables floating mathematical notation like f(x) = 2*pi g(x) ^ h(x) + sin (k(x))
instance Floating ExprTD where
  pi            = Const pi
  exp           = Exp
  log           = Log
  sqrt          = Sqrt
  (**) e e'     = e :**: e'
  logBase       = LogBase
  sin           = Sin
  cos           = Cos
  tan           = Tan
  asin          = undefined
  acos          = undefined
  atan          = undefined
  sinh          = undefined
  cosh          = undefined
  tanh          = undefined
  asinh         = undefined
  acosh         = undefined
  atanh         = undefined


-- | Like should probably use lookup tables like in the course.
transform :: ExprTD -> ExprFD
transform = undefined

-- | Partial fraction decomposition.
toPartial :: ExprFD -> Maybe ExprFD
toPartial = undefined

-- | Simplify strategy.
-- Multiply out brackets.
-- Apply cool exponant rules.
-- Combine like terms.
-- Combine contant term.

-- | Pushes multiplication down. TODO fix.
distribute (Pow n)                             = Pow n
distribute (Const s)                           = Const s
distribute (Const s :*: Const s')              = Const (s*s')
distribute ((Const s) :*: (Const s' :*: e))    = distribute (Const (s*s') :*: e)
distribute ((Const s) :*: (e :*: Const s'))    = distribute (Const (s*s') :*: e)
distribute ((Const s) :*: (e :+: e'))          = distribute (Const s :*: e) :+: distribute (Const s :*: e')
distribute (e :+: e')                          = (distribute e) :+: (distribute e')
distribute e = e

combine (Pow s)                                         = Pow s
combine (Const s)                                       = Const s
combine (Const s :+: Const s')                          = Const (s+s')
combine (Const s :+: (Const s' :+: e) )                 = combine $ Const (s+s') :+: e
combine (Const s :+: (e :+: Const s') )                 = combine $ Const (s+s') :+: e
combine ((Const s' :+: e) :+: Const s )                 = combine $ Const (s+s') :+: e
combine ((e :+: Const s') :+: Const s )                 = combine $ Const (s+s') :+: e
combine e@((Const s :*: Pow n) :+: (Const s' :*: Pow n'))   | n == n'   = Const(s+s') :*: Pow n
                                                            | otherwise = e
combine (e :+: e')                                      = combine e :+: combine e'
combine e                                               = e

shift (Pow s)                                       = Pow s
shift (Const s)                                     = Const s
shift (Const s :+: e)                               = Const s   :+: shift e
shift (e :+: Const s)                               = Const s   :+: shift e
shift ((Const s :+: e) :+: e'@(Const _ :*: Pow _))  = Const s   :+: shift (e :+: e')
shift ((Const s :+: e) :+: e')                      = Const s   :+: e' :+: e
shift ((e' :+: e'') :+: e''')                       = shift (e' :+: (e'' :+: e'''))
shift (Pow n :+: Pow n')    | n > n'                = Pow n'    :+: Pow n
                            | otherwise             = Pow n     :+: Pow n'
shift e                                             = e



-- | Simplifies an expression.
--simplifyExpr :: Fractional (Expr a) => Expr a -> Expr a
simplifyExpr (Pow 1)                    = Pow 1
simplifyExpr (Const c)                  = Const c
simplifyExpr e@((Pow n) :+: (Pow n'))
    | n == n' = 2 * (Pow n)
    | otherwise = e
simplifyExpr ((Const c) :+: (Const c')) = Const (c + c')
simplifyExpr (e :+: e')                 = simplifyExpr e + simplifyExpr e'
simplifyExpr ((Const c) :-: (Const c')) = Const (c - c')
simplifyExpr (e :-: e')                 = simplifyExpr e - simplifyExpr e'
simplifyExpr ((Pow n) :*: (Pow n'))     = Pow (n + n')
simplifyExpr ((Const 0) :*: _)          = 0
simplifyExpr (_ :*: (Const 0))          = 0
simplifyExpr e@((Const c) :*: (Pow n))  = e
simplifyExpr e@((Pow n) :*: (Const c))  = e
simplifyExpr ((Const c) :*: e')         = fmap (*c) (simplifyExpr e')
simplifyExpr (e :*: (Const c))          = fmap (*c) (simplifyExpr e)
simplifyExpr (e :*: e')                 = simplifyExpr e * simplifyExpr e'
simplifyExpr ((Const 0) :**: (Const 0)) = error "0^0 is not defined."
simplifyExpr (_ :**: (Const 0))         = 1
simplifyExpr ((Const 0) :**: _)         = 0
simplifyExpr (e :**: e')                = undefined
simplifyExpr (LogBase e e')             = undefined
simplifyExpr (Neg e)                    = undefined
simplifyExpr (Abs e)                    = undefined
simplifyExpr (Signum e)                 = undefined
simplifyExpr (Exp e)                    = undefined
simplifyExpr (Cos e)                    = undefined
simplifyExpr (Sin e)                    = undefined
simplifyExpr (Tan e)                    = undefined
simplifyExpr (Sqrt e)                   = undefined
simplifyExpr (Log e)                    = undefined

--deriveExpr :: Fractional (Expr a) => Expr a -> Expr a
deriveExpr (Pow 1)        = Const 1
deriveExpr (Pow n)        = undefined -- n * (Pow (n-1))
deriveExpr (Const c)      = Const c
deriveExpr (e :/: e')     = (e * (deriveExpr e') - (deriveExpr e) * e') / (e'^2)
deriveExpr (e :+: e')     = (deriveExpr e) + (deriveExpr e')
deriveExpr (e :-: e')     = (deriveExpr e) - (deriveExpr e')
deriveExpr (e :*: e')     = (e * deriveExpr e' + deriveExpr e * e')
deriveExpr (e :**: e')    = undefined
deriveExpr (LogBase e e') = undefined
deriveExpr (Neg e)        = undefined
deriveExpr (Abs e)        = undefined
deriveExpr (Signum e)     = undefined
deriveExpr (Exp e)        = undefined
deriveExpr (Cos e)        = undefined
deriveExpr (Sin e)        = undefined
deriveExpr (Tan e)        = undefined
deriveExpr (Sqrt e)       = undefined
deriveExpr (Log e)        = undefined

-- | Semantics of DSL. Evaluates a expression, returning a new function.
evalExpr :: Floating t => Expr t -> t -> t
evalExpr (Pow 1)        = id
evalExpr (Pow n)        = \s -> s ** fromInteger n
evalExpr (Const c)      = const c
evalExpr (e :/: e')     = \s -> ((evalExpr e) s) / ((evalExpr e') s)
evalExpr (e :+: e')     = \s -> (evalExpr e) s + (evalExpr e') s
evalExpr (e :-: e')     = \s -> (evalExpr e) s - (evalExpr e') s
evalExpr (e :*: e')     = \s -> (evalExpr e) s * (evalExpr e') s
evalExpr (e :**: e')    = \s -> (evalExpr e) s ** (evalExpr e') s
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

-- | f(t) = 2(2 + 2(2 + x)) + 2 + 2x
example :: ExprTD
example = 2*(2 + 2*(2 + idE)) + 2 + idE

-- | f(t) = 2x + 3(2 + x^2)
example1 :: ExprTD
example1 = 2 * idE + (2 + idE * idE)*3

test = evalExpr example $ 2 -- == 16

