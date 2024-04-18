import Prelude hiding ((+), (-), (*), (^), (*>), (<*))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring                  
import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: QSpray 
y = lone 2 :: QSpray 
z = lone 3 :: QSpray
poly  = ((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^-^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
poly' = ((2%3) *> (x^3 * y * z) - x^2) * ((7%4) *> (x * y * z))
