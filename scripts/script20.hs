import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational

sprayD = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  z  ^+^  constantSpray 3
sprayA = sprayD^**^1 ^*^ (x^**^4  ^-^  x  ^+^  y   ^+^  x ^*^ y ^*^ z^**^2)
sprayB = sprayD^**^1 ^*^ y ^*^ (2*^x  ^+^  unitSpray) ^*^ z

g = gcdQspray sprayA sprayB

