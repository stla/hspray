import Math.Algebra.Hspray


n = 3
nAsSpray = constantSpray (toRational n)
g   = gegenbauerPolynomial n
g'  = derivSpray 1 g
g'' = derivSpray 1 g'

alpha = lone 1 :: Spray Rational
x     = lone 1 :: Spray (Spray Rational)

test = 
  (unitSpray ^-^ x^**^2) ^*^ g''
  ^-^ (2.^alpha ^+^ unitSpray) *^ (x ^*^ g')
  ^+^ n.^(nAsSpray ^+^ 2.^alpha) *^ g
