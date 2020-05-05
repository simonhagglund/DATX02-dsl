module Complex where



type R = Double


data Complex = Complex (R, R)
             | RealInf
             | CompInf
             | ImagInf
    deriving (Show)


whatsReal :: Complex -> R
whatsReal (Complex (x,y)) = x

whatsImaginary :: Complex -> R
whatsImaginary (Complex (x,y)) = y


add :: Complex -> Complex -> Complex
add (Complex (r1,i1)) (Complex (r2,i2)) = Complex (r1+r2, i1+i2)

sub :: Complex -> Complex -> Complex
sub (Complex (r1,i1)) (Complex (r2,i2)) = Complex (r1-r2, i1-i2)

absolute :: Complex -> Complex -- returns complex number z with im(z) = 0
absolute (Complex (real,imaginary)) = Complex (sqrt (real**2 + imaginary**2),0)

argument :: Complex -> Complex -- returns complex number z with im(z) = 0
argument (Complex (real,imaginary)) 
  | real < 0 && imaginary > 0  = Complex ((atan (imaginary/real)) + pi, 0)
  | real < 0                   = Complex ((atan (imaginary/real)) - pi, 0)
  | otherwise                  = Complex (atan (imaginary/real),        0)

multiply :: Complex -> Complex -> Complex
multiply (Complex (r1,i1)) (Complex (r2,i2))
  = Complex (r1*r2 - i1*i2, r1*i2 + r2*i1)


divide :: Complex -> Complex -> Complex
divide z1 z2 = (multiply z1 (conjugate z2)) / abs(z2)

fromInt :: Integer -> Complex
fromInt c = Complex (fromInteger c,0)

sign :: Complex -> Complex 
sign z = undefined 

conjugate :: Complex -> Complex 
conjugate (Complex (x, y)) = Complex (x, negate y)

instance Num Complex where
  (+)         = add
  (-)         = sub
  (*)         = multiply
  abs         = absolute
  negate      = multiply (Complex (-1,0))
  fromInteger = fromInt

instance Fractional Complex where
  (/) = divide 




main = undefined 
