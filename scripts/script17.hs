import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
sprayA = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
sprayB = x ^*^ y ^-^ (2*^ y^**^2)
(sprayQ, sprayR) = multivariateDivision sprayA sprayB
test = sprayB ^*^ sprayQ ^+^ sprayR

