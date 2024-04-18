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

rOS3 = rOS2 - unitRatioOfSprays/rOS2
num1 = _numerator rOS1
den1 = _denominator rOS1
num3 = _numerator rOS3
den3 = _denominator rOS3
num = num1 * num3
den = den1 * den3
g = gcdSpray num den


rOS = rOS1^2 + rOS1*rOS2 - rOS1/rOS2 + rOS2



test1 = (rOS1 + rOS2) * (rOS1 - rOS2) == rOS1^2 - rOS2^2

rOS' = (3%4 :: Rational) *> rOS^2  +  p *> rOS

test2 = p *> (rOS' /> p) == rOS'
test3 = rOS1 /> p == p %//% q

f :: Algebra.Field.C a => a -> a -> a
f u v = u^2 + u*v - u/v + v
test4 = rOS == f rOS1 rOS2
values = [2%3, 7%4]
r1 = evalRatioOfSprays rOS1 values
r2 = evalRatioOfSprays rOS2 values
test5 = evalRatioOfSprays rOS values == f r1 r2

tests = [test1, test2, test3, test4, test5]