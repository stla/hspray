import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
p = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
q = x ^-^ (2*^ y^**^2)
f = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^x 
g = x ^-^ (2*^ x^**^2) ^+^ constantSpray 4