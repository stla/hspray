import Prelude hiding ((*), (+), (-), (/), (^), (*>))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module            
import Algebra.Ring
import Algebra.Field                
import Math.Algebra.Hspray
import Number.Ratio ( (%), T ( (:%) ) )
-- import qualified MathObj.Polynomial            as MP
-- import qualified Number.Ratio                  as NR

a    = qpolyFromCoeffs [0, 1]  
p    = a^2 - constQPoly 4 
q1   = a - constQPoly 3
q2   = a - constQPoly 2
rop1 = p :% q1 
rop2 = p :% q2

f :: (Eq a, Algebra.Ring.C a) => Spray a -> Spray a -> Spray a -> (Spray a, Spray a)
f x y z = (x*x + y*y, z)

g :: (Eq a, Algebra.Ring.C a) => Spray a -> Spray a -> Spray a -> (a, a, a) -> (a, a)
g px py pz (x, y, z) = (evalSpray f1 [x, y, zero], evalSpray f2 [zero, zero, z])
  where (f1, f2) = f px py pz

px = lone 1 :: SymbolicQSpray 
py = lone 2 :: SymbolicQSpray 
pz = lone 3 :: SymbolicQSpray 

x = lone 1 :: Spray Rational' -- alias QSpray !
y = lone 2 :: Spray Rational' 
z = lone 3 :: Spray Rational' 

(test1, test2) = g x y z (2, 3, 4) 
test = evalRatioOfPolynomials 5 rop1 * test1  +  evalRatioOfPolynomials 5 rop2 * test2

(f1, f2) = f px py pz
symSpray  = rop1 *^ f1  +  rop2 *^ f2 
check = evalSymbolicSpray' symSpray 5 [2, 3, 4]
