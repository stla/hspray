import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ (4 *^ (x ^*^ y ^*^ z))
-- prettySpray show "x" poly
-- "(4.0) * x^(3, 1, 1) + (8.0) * x^(4, 2, 2)"
