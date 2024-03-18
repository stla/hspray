import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
p = x^**^2 ^*^ y ^*^ (y^**^2 ^-^ 5*^ x ^+^ constantSpray 6) 
q = x^**^2 ^*^ y ^*^ (3*^ y ^+^ constantSpray 2)

sx = map prettySpray' (subresultants 1 p q) 
sy = map prettySpray' (subresultants 2 p q)