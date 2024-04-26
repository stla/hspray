import Math.Algebra.Hspray
x = qlone 1

sprayA = (x^**^3 ^-^ 4 *^ x^**^2 ^+^ 5 *^ x)
sprayB = (x ^-^ 6 *^ x) 

(qA, qB) = quotientsByGCD sprayA sprayB

g = gcdSpray sprayA sprayB
((qA', _), (qB', _)) = (sprayDivision sprayA g, sprayDivision sprayB g)


sprayA' = (x^**^4 ^-^ 4 *^ x^**^2 ^+^ 5 *^ x)
sprayB' = (x ^-^ 6 *^ x) 

(qA'', qB'') = quotientsByGCD sprayA' sprayB'

g' = gcdSpray sprayA' sprayB'
((qA''', _), (qB''', _)) = (sprayDivision sprayA' g', sprayDivision sprayB' g')


