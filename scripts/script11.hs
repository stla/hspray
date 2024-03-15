import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
p = x^**^2 ^+^ (5 *^ y) ^+^ z ^-^ unitSpray
p' = permuteVariables p [3, 1, 2]