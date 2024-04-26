import Math.Algebra.Hspray
x = qlone 1
y = qlone 2

sprayA = (x^**^3 ^-^ 4 *^ x^**^2 ^+^ 5 *^ x) ^*^ y^**^2
sprayB = (x^**^2 ^-^ 6 *^ x) ^*^ y

(qA, qB) = quotientsByGCD sprayA sprayB

g = gcdSpray sprayA sprayB
((qA', _), (qB', _)) = (sprayDivision sprayA g, sprayDivision sprayB g)


