import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
symSpray = (x^**^2 ^+^ y^**^2) ^+^ (x ^+^ y)
p1 = psPolynomial 2 1 :: Spray Rational
p2 = psPolynomial 2 2 :: Spray Rational
(check, maybeP) = isPolynomialOf symSpray [p1, p2]
