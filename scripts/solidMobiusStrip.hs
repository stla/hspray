module X where 
import Prelude hiding ((*), (+), (-))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring                  
import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray (Spray Rational)
y = lone 2 :: Spray (Spray Rational)
z = lone 3 :: Spray (Spray Rational)
a = lone 1 :: Spray Rational
b = lone 2 :: Spray Rational

poly =  a *^ (x*x + y*y) + ((2%3) *^ b) *^ z 

ps = prettySpray (prettySpray show "a") "X" poly

l = toList poly
pows = map fst l
coeffs = map toList $ map snd l