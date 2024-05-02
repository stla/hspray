import Math.Algebra.Hspray

cost = qlone 1
sint = qlone 2
n_variables = 2
a = qlone 3
b = qlone 4
equations = [a ^*^ cost, b ^*^ sint]
relations = [cost^**^2 ^+^ sint^**^2 ^-^ unitSpray]
m = maximum (map numberOfVariables equations)
n_equations = length equations
coordinates = [qlone (m + i) | i <- [1 .. n_equations]]
generators = relations ++ zipWith (^-^) equations coordinates 
gb = groebnerBasis generators True
-- gb' = drop 1 gb
isfree :: QSpray -> Bool
isfree spray = not $ or (map (involvesVariable spray) [1 .. n_variables]) 
results = filter isfree gb
results' = map (dropVariables n_variables) results 
showResults = map (prettyQSprayXYZ ["a", "b", "x", "y"]) results'

{- sprays = generators
j0       = length sprays
ltsprays       = map leadingTerm sprays
spraysltsprays = zip sprays ltsprays 
spraysMap      = HM.fromList (zip [0 .. j0-1] spraysltsprays)

gpolysMap = spraysMap
sfg      = sPolynomial (gpolysMap HM.! 0) (gpolysMap HM.! 1)
sbarfg   = sprayDivisionRemainder' sfg gpolysMap
ltsbarfg = leadingTerm sbarfg
 -}