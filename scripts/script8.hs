import Math.Algebra.Hspray
import Data.Ratio

-- define the elementary monomials
o = lone 0 :: Spray Rational
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational

-- define three polynomials
p1 = x^**^2 ^+^ y ^+^ z ^-^ o -- x² + y + z - 1
p2 = x ^+^ y^**^2 ^+^ z ^-^ o -- x + y² + z - 1
p3 = x ^+^ y ^+^ z^**^2 ^-^ o -- x + y + z² - 1

-- compute the Gröbner basis
g = groebner [p1, p2, p3]

-- show result
prettyResult = map (prettySpray show "x") g
-- mapM_ print prettyResult
-- "((-1) % 1) * x^() + (1 % 1) * x^(0, 0, 2) + (1 % 1) * x^(0, 1) + (1 % 1) * x^(1)"
-- "(1 % 1) * x^(0, 0, 1) + ((-1) % 1) * x^(0, 0, 2) + ((-1) % 1) * x^(0, 1) + (1 % 1) * x^(0, 2)"
-- "((-1) % 2) * x^(0, 0, 2) + (1 % 2) * x^(0, 0, 4) + (1 % 1) * x^(0, 1, 2)"
-- "((-1) % 1) * x^(0, 0, 2) + (4 % 1) * x^(0, 0, 3) + ((-4) % 1) * x^(0, 0, 4) + (1 % 1) * x^(0, 0, 6)"
