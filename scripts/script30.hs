import Math.Algebra.Hspray
import qualified Data.Foldable       as DF
import qualified Data.HashMap.Strict           as HM
import qualified Data.Sequence           as S

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
gb' = drop 1 gb
f :: Exponents -> Bool
f expnts = S.null expnts || DF.all (0 ==) (S.take n_variables expnts)
isfree :: QSpray -> Bool
isfree spray = let allExponennts = map exponents (HM.keys spray) in 
    DF.all f allExponennts
results = filter isfree gb'
dropXis = HM.mapKeys 
            (\(Powers exps n) -> 
                Powers (S.drop n_variables exps) (n - n_variables))
results' = map dropXis results
showResults = map (prettyQSprayXYZ ["a", "b"]) results'

sprays = generators
j0       = length sprays
ltsprays       = map leadingTerm sprays
spraysltsprays = zip sprays ltsprays 
spraysMap      = HM.fromList (zip [0 .. j0-1] spraysltsprays)

gpolysMap = spraysMap
sfg      = sPolynomial (gpolysMap HM.! 0) (gpolysMap HM.! 1)
sbarfg   = sprayDivisionRemainder' sfg gpolysMap
ltsbarfg = leadingTerm sbarfg
