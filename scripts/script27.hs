import Prelude hiding ((*), (+), (-), (/), (^), (*>), const, fromRational)
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module            
import Algebra.Ring
import Algebra.Field                
import Math.Algebra.Hspray
-- import Data.Ratio
import MathObj.Polynomial (const)
import Number.Ratio ( (%), T ( (:%) ) )
import qualified MathObj.Polynomial            as MP
import qualified Number.Ratio                  as NR

a = qpolyFromCoeffs [0, 1] :: QPolynomial 
p = a^2 - constQPoly 4 
q = a - constQPoly 3
q' = a - constQPoly 2
poverq = p :% q 
poverq' = p :% q'

x = lone 1 :: SymbolicQSpray 
y = lone 2 :: SymbolicQSpray 
z = lone 3 :: SymbolicQSpray 

symSpray = poverq *^ (x*x + y*y) + ((scalarQ (2 % 3)) *> poverq') *^ z -- définir un opérateur ?
symSpray' = poverq *^ (x*x + y*y) + ((2 % 3) *. poverq') *^ z 

psymSpray = prettySymbolicSpray "a" symSpray
psymSpray' = prettySymbolicSpray "a" symSpray'

