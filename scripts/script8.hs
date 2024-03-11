import Math.Algebra.Hspray
import Data.Ratio
o = lone 0 :: Spray Rational
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
p1 = x^**^2 ^+^ y ^+^ z ^-^ o
p2 = x ^+^ y^**^2 ^+^ z ^-^ o
p3 = x ^+^ y ^+^ z^**^2 ^-^ o
g = groebner3 [p1, p2, p3]
result = map (prettySpray show "x") g
