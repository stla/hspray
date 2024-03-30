import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational

p = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ z^**^4 
q = x ^-^ (2*^ y^**^2) ^*^ z^**^2 ^*^ unitSpray

rx  = resultant 1 p q
rx' = resultant' 1 p q
ry  = resultant 2 p q
ry' = resultant' 2 p q
rz  = resultant 3 p q
rz' = resultant' 3 p q

test1 = rx == rx'
test2 = ry == ry'
test3 = rz == rz'

test = test1 && test2 && test3



