import Math.Algebra.Hspray
import Data.Ratio
import qualified MathObj.Polynomial            as MP
import qualified Number.Ratio                  as NR
import qualified Algebra.Additive                  as AA
import qualified Algebra.Field                  as AF
import qualified Algebra.Ring                  as AR


x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational

p = MP.fromCoeffs [6, 0, 0] :: QPolynomial
q = MP.fromCoeffs [0, 3, 0] :: QPolynomial
poverq = p NR.:% q
qoverp = q NR.:% p
