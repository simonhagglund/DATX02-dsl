module LaplaceTransform where

import Expression 
import ComplexNumbers 



type TimeExpression      = Expression Complex -- TODO: make this Expression Real without bugs? 
type FrequencyExpression = Expression Complex 

laplace :: TimeExpression -> FrequencyExpression 
-- laplace :: Expression Complex -> Expression Complex  
-- linearity
laplace (e1 :+: e2)      = laplace e1 + laplace e2
laplace (Const a :*: e1) = Const a :*: laplace e1

-- some simple transforms 
laplace Impulse          = 1 
laplace (Const 1)        = 1 :/: Id -- assumes t > 0 
laplace (Const a)        = Const a :*: laplace (Const 1) -- Const a :/: Id  ?

-- Dämpningssatsen TODO: maybe implement commutation better? 
laplace (e1 :*: Exp (Const a :*: Id)) = Shift a $ laplace e1 
laplace (Exp (Const a :*: Id) :*: e1) = Shift a $ laplace e1 
laplace (Exp ((Const a) :*: Id))      = (Const 1) :/: (Id :+: (Const (-a)))-- L{t e^-at} = 1/(s-a)

-- L{t} = 1/s^2
laplace Id                            = 1 :/: (Id :*: Id)

-- L{e^at - e^bt} = (b-a)/((s+a)(s+b))
laplace (Exp (Const a :*: Id) :-: Exp (Const b :*: Id)) = (Const b :-: Const a) :/: ((Id :+: Const a) :*: (Id :+: Const b))

-- L{1 - e^-at} = a/(s(s+a)); is there prettier way to implement? Where to put negate? 
laplace (Const 1 :-: Exp (Const a :*: Id)) = Negate $ Const a :/: (Id :*: (Id :-: Const a))

-- how to do (s+a)^2 etc?
-- laplace (Exp (Const a :*: Id) :*: Sin (Const omega :*: Id)) = Const omega :/: ((Id :+: Const a)^2 + Id^2)
laplace _ = error "laplace: not implemented (yet)" 


-- invers 

laplaceinv :: FrequencyExpression -> TimeExpression 
laplaceinv (e1 :+: e2) = laplaceinv e1 :+: laplaceinv e2
laplaceinv (Const 1)   = Impulse 
laplaceinv (Const a)   = Const a :*: Impulse
laplaceinv (Const a :*: e1) = Const a :*: laplaceinv e1 
laplaceinv (Const 1 :/: Id) = Const 1
laplaceinv _ = error "laplaceinv: not implemented (yet)"




