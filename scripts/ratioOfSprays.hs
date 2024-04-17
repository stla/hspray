import Prelude hiding ((*), (+), (-), (^))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring                  
import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: QSpray 
y = lone 2 :: QSpray

p = x^4 - y^4 
q = x - y

rOS = p %//% q
