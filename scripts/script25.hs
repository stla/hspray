import Prelude hiding ((*), (+), (-), (/), (^), (*>), const, fromRational)
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module            
import Algebra.Ring
import Algebra.Field                
import Math.Algebra.Hspray
import Data.Ratio
import MathObj.Polynomial (const)
import Number.Ratio ( T ( (:%) ) )
import qualified MathObj.Polynomial            as MP
import qualified Number.Ratio                  as NR

a = MP.fromCoeffs [0, 1] :: QPolynomial
p = a^2 - const 4 
q = a - const 2
poverq = p :% q 

x = lone 1 :: Spray RatioOfQPolynomials
y = lone 2 :: Spray RatioOfQPolynomials
z = lone 3 :: Spray RatioOfQPolynomials

poly = poverq *^ (x*x + y*y) + ((2%3::Rational) *> poverq) *^ z 

