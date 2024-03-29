import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
d = y ^+^ constantSpray 2
sprayA = d ^*^ x^**^4  ^-^  d^*^y ^*^ x^**^3  ^-^  d ^*^ x  ^+^  d 

coeffs = sprayCoefficients sprayA
coeffs' = map (swapVariables (1, 2)) coeffs
contA = foldl1 gcdQX coeffs'

exactDivisionBy :: Spray Rational -> Spray Rational -> Spray Rational
exactDivisionBy b a = fst $ multivariateDivision a b 

coeffs'' = map (exactDivisionBy contA) coeffs'
coeffs''' = map (swapVariables (1, 2)) coeffs''

sprayX = lone 1
xmonoms = map (\k -> sprayX^**^k) [4, 3 .. 0]

final = foldl1 (^+^) $ zipWith (^*^) coeffs''' xmonoms

content :: Spray Rational -> Spray Rational
content spray = foldl1 gcdQX coeffs'
  where
    coeffs   = sprayCoefficients spray
    coeffs'  = map (swapVariables (1, 2)) coeffs

reduceSpray :: Spray Rational -> Spray Rational
reduceSpray spray = foldl1 (^+^) $ zipWith (^*^) coeffs'' xmonoms
  where
    coeffs   = sprayCoefficients spray
    coeffs'  = map (swapVariables (1, 2)) coeffs
    cntnt    = foldl1 gcdQX coeffs'
    coeffs'' = map (swapVariables (1,2) . (exactDivisionBy cntnt)) coeffs'
    sprayX  = lone 1
    deg     = length coeffs - 1
    xmonoms = map (\k -> sprayX^**^k) [deg, deg-1 .. 0]

reduceSpray' :: Spray Rational -> Spray Rational -> Spray Rational
reduceSpray' spray divisor = foldl1 (^+^) $ zipWith (^*^) coeffs'' xmonoms
  where
    coeffs   = sprayCoefficients spray
    coeffs'  = map (swapVariables (1, 2)) coeffs
    -- divisor' = swapVariables (1, 2) divisor
    coeffs'' = map (swapVariables (1,2) . (exactDivisionBy divisor)) coeffs'
    sprayX  = lone 1
    deg     = length coeffs - 1
    xmonoms = map (\k -> sprayX^**^k) [deg, deg-1 .. 0]

