import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
d = y ^+^ constantSpray 2
sprayA = d ^*^ x^**^4  ^-^  d^*^y ^*^ x^**^3  ^-^  d ^*^ x  ^+^  d 
coeffs = sprayCoefficients sprayA
coeffs' = map (\p -> swapVariables p (1, 2)) coeffs
contA = foldl1 gcdQX coeffs'

exactDivision :: Spray Rational -> Spray Rational -> Spray Rational
exactDivision a b = q 
  where
    (q, _) = multivariateDivision a b

coeffs'' = map (\p -> exactDivision p contA) coeffs'



