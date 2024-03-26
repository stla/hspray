import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
sprayA = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
sprayB = x ^*^ y ^-^ (2*^ y^**^2)
(c, (sprayQ, sprayR)) = pseudoDivision sprayA sprayB
test = sprayB ^*^ sprayQ ^+^ sprayR
-- to check: c ^*^ sprayA == test

usprayA = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ x^**^2) ^+^ x^**^4 
usprayB = x ^-^ (2*^ x^**^2) ^+^ constantSpray 5
(uc, (usprayQ, usprayR)) = pseudoDivision usprayA usprayB
utest = usprayB ^*^ usprayQ ^+^ usprayR

-- bivarié ok
-- univarié : erreur si B divise A