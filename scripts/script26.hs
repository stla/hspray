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

a = MP.fromCoeffs [Q 0, Q 1] :: QPolynomial
p = a^2 - const (Q 4) 
q = a - const (Q 3)
q' = a - const (Q 2)
poverq = p :% q 
poverq' = p :% q'

x = lone 1 :: SymbolicSpray
y = lone 2 :: SymbolicSpray
z = lone 3 :: SymbolicSpray

symSpray = poverq *^ (x*x + y*y) + ((Q (2 % 3)) *> poverq') *^ z 

psymSpray = prettySymbolicSpray "a" symSpray
psymSpray' = prettySymbolicSpray "a" (simplifySymbolicSpray symSpray)

