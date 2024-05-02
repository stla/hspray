import Math.Algebra.Hspray

x1 = qlone 1
x2 = qlone 2
x3 = qlone 3
x4 = qlone 4
sprays = [
    esPolynomial 4 1
  , x1^*^x2 ^+^ x1^*^x3 ^+^ x1^*^x4 ^+^ x2^*^x3
  , esPolynomial 4 3
  , esPolynomial 4 4 ]
generators = zipWith (^-^) sprays [qlone (4 + i) | i <- [1 .. 4]]

gbasis = groebnerBasis generators False

-- CONCLUSION:
-- This is not a bug. It just takes a long time. There are 81 polynomials
-- in the non-minimal basis, and 48 in the minimal basis.
