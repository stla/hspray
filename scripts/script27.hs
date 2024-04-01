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

a = qpolyFromCoeffs [0, 1] :: QPolynomial NR.Rational
p = a^2 - constQPoly 4 
q = a - constQPoly 3
q' = a - constQPoly 2
poverq = p :% q 
poverq' = p :% q'

x = lone 1 :: SymbolicSpray NR.Rational
y = lone 2 :: SymbolicSpray NR.Rational
z = lone 3 :: SymbolicSpray NR.Rational

symSpray = poverq *^ (x*x + y*y) + ((Q (2 % 3) :: Q NR.Rational) *> poverq') *^ z 

psymSpray = prettySymbolicSpray "a" symSpray
psymSpray' = prettySymbolicSpray "a" (simplifySymbolicSpray symSpray)

