import Math.Algebra.Hspray
import Data.Ratio
x1 = lone 1 :: Spray Rational
x2 = lone 2 :: Spray Rational
x3 = lone 3 :: Spray Rational
p = x1^**^2 ^+^ x2 ^+^ x3 ^-^ unitSpray
prettySpray' p
-- "((-1) % 1) + (1 % 1) x3 + (1 % 1) x2 + (1 % 1) x1^2"
--
-- substitute x1 -> 2 and x3 -> 3
p' = substituteSpray [Just 2, Nothing, Just 3] p
prettySpray' p'
-- "(6 % 1) + (1 % 1) x2"