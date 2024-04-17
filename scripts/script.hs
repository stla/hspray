import           Prelude hiding ((*), (+), (-), (/), (^), (*>))
import qualified Prelude as P
import           Algebra.Additive              
import           Algebra.Module            
import           Algebra.Ring
import           Algebra.Field                
import           Math.Algebra.Hspray

x = lone 1 :: QSpray 
y = lone 2 :: QSpray 

rOS = (x^**^4 ^-^ y^**^4) %//% (x ^-^ y) 
