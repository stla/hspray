import Math.Algebra.Hspray
import Data.Ratio

-- define the elementary monomials
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational

-- 
p = x^**^2 ^+^ y ^+^ z ^-^ unitSpray -- x² + y + z - 1
p' = substituteSpray [Just 2, Nothing, Just 3] p
