import Prelude hiding ((+), (-), (*), (/), (^), (*>), (<*))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module
import Algebra.RightModule
import Algebra.Ring
import Algebra.Field              
import Math.Algebra.Hspray
import Data.Ratio ( (%) )

x = qlone 1  
y = qlone 2 
p = x^2 - 3*^(x * y) + y^3 
q = x - y

rOS1 = p^2 %//% q
rOS2 = rOS1 + unitRatioOfSprays
rOS = rOS1^2 + rOS1*rOS2 - rOS1

test1 = (rOS1 + rOS2) * (rOS1 - rOS2) == rOS1^2 - rOS2^2

rOS' = (3%4 :: Rational) *> rOS^2  +  p *> rOS

test2 = p *> (rOS' /> p) == rOS'
test3 = rOS1 /> p == p %//% q
