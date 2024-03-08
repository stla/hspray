import Math.Algebra.Hspray
import Data.Ratio
o = lone 0 :: Spray Rational
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
poly = (x^**^3 ^*^ y^**^2) ^+^ x ^*^ y ^+^ x
p1 = y^**^2 ^+^ o
p2 = x ^*^ y ^+^ o
r = bbDivision poly [p1, p2]
-- prettySpray show "x" poly
-- "(7 % 4) * x^(3, 1, 1) + (7 % 6) * x^(4, 2, 2)"
