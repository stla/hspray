import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
poly = ((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^-^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
