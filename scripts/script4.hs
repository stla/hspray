import           Prelude hiding ((*), (+), (-), (/), (^), (*>))
import qualified Prelude as P
import           Algebra.Additive              
import           Algebra.Module            
import           Algebra.Ring
import           Algebra.Field
import           Math.Algebra.Hspray
import           Number.Ratio       ( (%), T ( (:%) ) )
x = lone 1 :: OneParameterQSpray 
y = lone 2 :: OneParameterQSpray 
z = lone 3 :: OneParameterQSpray
a = qsoleParameter
spray 
  = ((4%5::Rational') *> (a :% (a^2 + one))) *> (x^2 + y^2)  
        +  ((2 .^ a) *> (y * z)) /> (3::Rational')
