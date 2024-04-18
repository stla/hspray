import Prelude hiding ((*), (+), (-), (^))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring                  
import Math.Algebra.Hspray

x = lone 1 :: Spray (Spray Rational)
y = lone 2 :: Spray (Spray Rational)
z = lone 3 :: Spray (Spray Rational)
a = lone 1 :: Spray Rational
b = lone 2 :: Spray Rational

poly = a *^ (x^2 + y^2) + ((2 *^ b) /^ 3) *^ z 
s = showSprayXYZ' (prettyQSprayXYZ ["a","b"]) ["X","Y","Z"] poly
