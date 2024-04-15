import Math.Algebra.Hspray
p = lone 1 :: Spray Double
x = lone 1 :: Spray (Spray Double)
y = lone 2 :: Spray (Spray Double)
poly = ((p *^ x) ^-^ (p *^ y)) ^**^ 2 
-- showSpray 
-- prettySpray (prettySpray show "a") "X" poly
-- "((1.0) * a^(2)) * X^(0, 2) + ((2.0) * a^(2)) * X^(1, 1) + ((1.0) * a^(2)) * X^(2)"
