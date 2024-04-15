import Math.Algebra.Hspray
import qualified Algebra.Additive   as AlgAdd
import qualified Algebra.Module     as AlgMod
import qualified Algebra.Ring       as AlgRing
import           Number.Ratio       ( T ( (:%) ) )
import qualified Number.Ratio       as NR
x = lone 1 :: SymbolicQSpray 
y = lone 2 :: SymbolicQSpray 
z = lone 3 :: SymbolicQSpray 
a = outerQVariable  
sSpray 
  = ((4 NR.% 5) *. (a :% (a AlgRing.^ 2 AlgAdd.+ AlgRing.one))) AlgMod.*> (x^**^2 ^-^ y^**^2)  
    ^+^  (constQPoly (2 NR.% 3) AlgRing.* a) AlgMod.*> (y ^*^ z)
string = prettySymbolicQSpray' "a" sSpray
string' = 
  "{ [ (4/5)*a ] %//% [ a^2 + 1 ] }*X^2 + { [ -(4/5)*a ] %//% [ a^2 + 1 ] }*Y^2 + { (2/3)*a }*Y.Z"