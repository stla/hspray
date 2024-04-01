import Math.Algebra.Hspray
import Data.Ratio
import qualified MathObj.Polynomial            as MP
import qualified Number.Ratio                  as NR
import qualified Algebra.Additive                  as AA
import qualified Algebra.Field                  as AF
import qualified Algebra.Ring                  as AR

xxx = MP.fromCoeffs [Q 0, Q 1]
x = QPolynomial xxx 
four = QPolynomial (MP.const (Q 4)) 
a = (x AR.* x AA.- four)
b = (x AA.- QPolynomial (MP.const (Q 2)))
aoverb = a NR.:% b :: QPolynomialsRatio

{- p = MP.fromCoeffs [Q 6, Q 0] :: QPolynomial
q = MP.fromCoeffs [Q 0, Q 3] :: QPolynomial
poverq = p NR.:% q
qoverp = q NR.:% p
 -}