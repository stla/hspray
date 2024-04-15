import Math.Algebra.Hspray
import Data.Ratio
import qualified Algebra.Additive              as AlgAdd
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
p1 = ((2%3) *^ x^**^3) ^*^ y  ^-^  x^**^2  ^+^  y ^*^ z  ^-^  (2%3) *^ unitSpray
p2 = (3%2) *^ p1
p3 = AlgAdd.negate $ 
  swapVariables (1, 3) $ 
    p2  ^+^  unitSpray  ^-^  (x^**^3 ^*^ y  ^-^  ((3%2) *^ x^**^2))

strings = 
  [
    prettyQSpray (zeroSpray ^*^ p1)
  , prettyQSpray p1
  , prettyQSpray (AlgAdd.negate p2)
  , prettyQSpray (p2  ^+^  lone 4)
  , prettyQSpray p3
  , " ---------- "
  , prettyQSprayX1X2X3 "a" (zeroSpray ^*^ p1)
  , prettyQSprayX1X2X3 "a" p1
  , prettyQSprayX1X2X3 "a" (p2  ^+^  lone 4)
  , prettyQSprayX1X2X3 "a" p3
  , " ---------- "
  , prettySpray (zeroSpray ^*^ p1)
  , prettySpray p1
  , prettySpray (p2  ^+^  lone 4)
  , prettySpray p3
  , " ---------- "
  , prettySpray'' "w" (zeroSpray ^*^ p1)
  , prettySpray'' "w" p1
  , prettySpray'' "w" (p2  ^+^  lone 4)
  , prettySpray'' "w" p3
  ]

strings' =
  [
    "0"
  , "(2/3)*x^3.y - x^2 + y.z - (2/3)"
  , "-x^3.y + (3/2)*x^2 - (3/2)*y.z + 1"
  , "x1^3.x2 - (3/2)*x1^2 + (3/2)*x2.x3 + x4 - 1"
  , "-(3/2)*x.y"
  , " ---------- "
  , "0"
  , "(2/3)*a1^3.a2 - a1^2 + a2.a3 - (2/3)"
  , "a1^3.a2 - (3/2)*a1^2 + (3/2)*a2.a3 + a4 - 1"
  , "-(3/2)*a1.a2"
  , " ---------- "
  , "0"
  , "(2 % 3)*x^3.y + ((-1) % 1)*x^2 + (1 % 1)*y.z + ((-2) % 3)"
  , "(1 % 1)*x1^3.x2 + ((-3) % 2)*x1^2 + (3 % 2)*x2.x3 + (1 % 1)*x4 + ((-1) % 1)"
  , "((-3) % 2)*x.y"
  , " ---------- "
  , "0"
  , "(2 % 3)*w^(3, 1) + ((-1) % 1)*w^(2) + (1 % 1)*w^(0, 1, 1) + ((-2) % 3)*w^()"
  , "(1 % 1)*w^(3, 1) + ((-3) % 2)*w^(2) + (3 % 2)*w^(0, 1, 1) + (1 % 1)*w^(0, 0, 0, 1) + ((-1) % 1)*w^()"
  , "((-3) % 2)*w^(1, 1)"  
  ]

