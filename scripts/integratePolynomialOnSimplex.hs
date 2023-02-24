import           Algebra.Ring                   ( C )
import           Data.List                      ( transpose )
import           Data.Matrix                    ( detLU
                                                , fromLists
                                                )
import           Data.Ratio
import           Math.Algebra.Hspray            ( (*^)
                                                , (.^)
                                                , Spray
                                                , (^**^)
                                                , (^*^)
                                                , (^+^)
                                                , (^-^)
                                                , bombieriSpray
                                                , composeSpray
                                                , constantSpray
                                                , lone
                                                , toList
                                                )

integratePolynomialOnSimplex
  :: (C a, Fractional a, Ord a) => Spray a -> [[a]] -> a
integratePolynomialOnSimplex p simplex =
  s * abs (detLU $ fromLists b) / (fromIntegral $ product [2 .. n])
 where
  v            = last simplex
  n            = length v
  b            = map (\column -> zipWith (-) column v) (take n simplex)
  vb           = zip v (transpose b)
  variables    = map lone [1 .. n]
  newvariables = map
    (\(vi, bi) ->
      (constantSpray vi) ^+^ foldl1 (^+^) (zipWith (*^) bi variables)
    )
    vb
  q      = composeSpray p newvariables
  qterms = toList $ bombieriSpray q
  s      = sum $ map f qterms
   where
    f (exponents, coef) = if d == 0
      then coef
      else coef / (fromIntegral $ product [n + 1 .. n + d])
      where d = sum exponents

simplex :: [[Rational]]
simplex = [[1, 1, 1], [2, 2, 3], [3, 4, 5], [3, 2, 1]]

x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational

poly :: Spray Rational
poly = x ^**^ 4 ^+^ y ^+^ 2 .^ (x ^*^ y ^**^ 2) ^-^ 3 .^ z

integral :: Rational
integral = integratePolynomialOnSimplex poly simplex
