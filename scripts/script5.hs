module X where 
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = 2 *^ (x ^*^ y ^*^ z) ^+^ (3 *^ x^**^2)
-- derivate with respect to x
prettySpray show "x" $ derivSpray 1 poly
-- "(2.0) * x^(0, 1, 1) + (6.0) * x^(1)"
