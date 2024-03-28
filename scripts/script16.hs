import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
sprayD = x^**^2 ^+^ unitSpray
sprayA = sprayD ^*^ (x^**^4 ^-^ x) 
sprayB = sprayD ^*^ (2*^x ^+^ unitSpray)

g = gcdQX sprayA sprayB

