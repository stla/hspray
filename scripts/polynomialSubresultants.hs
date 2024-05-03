import Math.Algebra.Hspray

x = qlone 1
y = qlone 2

p = x^**^3 ^+^ x^**^2 ^+^ 3*^x ^+^ unitSpray
q = x^**^4 ^+^ 2*^x^**^3 ^-^ x
srs1 = subresultants1 p q
srs1' = polynomialSubresultants 1 p q

p' = x^**^3^*^y^**^3 ^+^ x^**^2 ^+^ 3*^x ^+^ unitSpray
q' = x^**^4 ^+^ 2*^x^**^3 ^-^ x^*^y^**^4
srs2 = subresultants 1 p' q'
srs2' = polynomialSubresultants 1 p' q'
srs3 = subresultants 2 p' q'
srs3' = polynomialSubresultants 2 p' q'


pspray = x^**^2^*^y ^+^ x^*^y^**^3 ^+^ y ^+^ unitSpray
qspray = x^**^3 ^+^ 2*^x^**^2 ^-^ x
srs = subresultants 2 pspray qspray 
