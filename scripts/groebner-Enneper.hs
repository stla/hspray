import Math.Algebra.Hspray
import Data.Ratio ( (%) )

u = qlone 1
v = qlone 2
x = 3*^u ^+^ 3*^(u ^*^ v^**^2) ^-^ u^**^3
y = 3*^v ^+^ 3*^(u^**^2 ^*^ v) ^-^ v^**^3
z = 3*^u^**^2 ^-^ 3*^v^**^2
generators = [x ^-^ qlone 3, y ^-^ qlone 4, z ^-^ qlone 5]
gbasis = groebnerBasis generators True
isfree :: QSpray -> Bool
isfree spray = not (involvesVariable spray 1) && not (involvesVariable spray 2)
results = filter isfree gbasis
results' = map (dropVariables 2) results 
showResults = map prettyQSpray results'

-- check
xyz = map (evaluateAt [1%4, 2%3]) [x, y, z]
equation = results' !! 0
shouldBeZero = evaluateAt xyz equation