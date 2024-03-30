import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: Spray (Spray Rational)
y = lone 1 :: Spray Rational

p = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ (2*^ y^**^2)*^ x ^+^ constantSpray y^**^4 
q = x ^-^ constantSpray (2*^ y^**^2)

r = resultant1 p q



