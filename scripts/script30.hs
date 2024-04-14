import Math.Algebra.Hspray
import Data.Ratio ( (%) )

x = lone 1 :: Spray Rational 
y = lone 2 :: Spray Rational 
z = lone 3 :: Spray Rational 
p = 2*^x ^+^ 3*^y^**^2 ^-^ (4%5)*^z^**^3 ^-^ 3*^unitSpray
