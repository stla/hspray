import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
poly = ((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
p1 = x^**^2 ^*^ y^**^2
p2 = x ^+^ (y ^*^ z^**^2)
r = bbDivision poly [p1, p2]
-- prettySpray show "x" poly
-- "(7 % 4) * x^(3, 1, 1) + (7 % 6) * x^(4, 2, 2)"
