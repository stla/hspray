import Prelude hiding ((*), (+), (-), (/), (^), (*>))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module            
import Algebra.Ring
import Algebra.Field                
import Math.Algebra.Hspray
import Number.Ratio ( (%), T ( (:%) ) )
x = lone 1 :: SymbolicQSpray 
y = lone 2 :: SymbolicQSpray 
z = lone 3 :: SymbolicQSpray 

a = qpolyFromCoeffs [0, 1]  
symbolicSpray 
  = ((4%5) *. (a :% (a^2 + a + one))) *> (x^2 + y^2)  +  (constQPoly (2%3) * a) *> (y * z)
