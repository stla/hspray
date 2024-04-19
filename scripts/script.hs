import qualified Algebra.Additive as AlgAdd
import qualified Algebra.Module as AlgMod
import qualified Algebra.Ring as AlgRing
import qualified Algebra.Field as AlgField
import Math.Algebra.Hspray

alpha0 = qlone 1
beta0 = qlone 2
cst = constantSpray
identify :: QSpray -> RatioOfQSprays
identify spray = RatioOfSprays spray unitSpray
x = lone 1 :: Spray RatioOfQSprays
p = (x ^-^ unitSpray)

t1 = identify (((alpha0 ^+^ cst 1)^*^(alpha0 ^+^ cst 2)) /^ 2)
t2 = identify (((alpha0 ^+^ cst 2)^*^(alpha0 ^+^ beta0 ^+^ cst 3)) /^ 2) 
t3 = identify (((alpha0 ^+^ beta0 ^+^ cst 3)^*^(alpha0 ^+^ beta0 ^+^ cst 4)) /^ 8) 

expected = t1 *^ unitSpray ^+^ t2 *^ p ^+^ t3 *^ p^**^2 
