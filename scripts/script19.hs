import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational

sprayD = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  constantSpray 3
sprayA = sprayD ^*^ (x^**^4  ^-^  x  ^+^  y^**^2) 
sprayB = sprayD ^*^ y ^*^ (2*^x  ^+^  unitSpray)

g = gcdQXY sprayA sprayB

