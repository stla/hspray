import Math.Algebra.Hspray
import Data.Ratio

n = 4
e2 = esPolynomial n 2 :: Spray Rational
e4 = esPolynomial n 3 :: Spray Rational
p = e2^**^2 ^+^ (2*^ e4)
indices = [1 .. n]
esPolys = map (\i -> esPolynomial n i :: Spray Rational) indices
yPolys = map (\i -> lone (n + i) :: Spray Rational) indices
gPolys = zipWith (^-^) esPolys yPolys
