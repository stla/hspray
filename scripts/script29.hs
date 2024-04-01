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

a = polyFromCoeffs [0, 1]  
symbolicSpray 
  = ((4%5) *. (a :% (a^2 + a + one))) *> (x^2 + y^2)  +  (constQPoly (2%3) * a) *> (y * z)

qpoly = ((a^8 - one) :% (a - one)) *> (unitSpray :: SymbolicQSpray)
--  prettySymbolicQSpray "a" qpoly
-- "((1) + (1)a + (1)a^2 + (1)a^3 + (1)a^4 + (1)a^5 + (1)a^6 + (1)a^7)*x1x2x3"

--  prettySpray' $ evalSymbolicSpray symbolicSpray (6%5)
-- "(24 % 91) x1^2 + (24 % 91) x2^2 + (4 % 5) x2x3"

--  evalSymbolicSpray' symbolicSpray (6%5) [2, 3, 4%7]
-- 24 % 5

-- prettyRatioOfQPolynomials "a" $ evalSymbolicSpray'' symbolicSpray [2, 3, 4%7]
-- [(404/35)a + (8/7)a^2 + (8/7)a^3] / [(1) + a + a^2]
