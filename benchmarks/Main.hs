module Main where
import Test.Tasty.Bench ( bench, bgroup, defaultMain, nf, whnf )
import Math.Algebra.Hspray
import qualified Algebra.Additive as AlgAdd
import qualified Algebra.Ring as AlgRing
import qualified Algebra.Module as AlgMod
import           Number.Ratio       ( (%), T ( (:%) ) )

f :: Integer -> RatioOfQPolynomials
f n = ((a AlgRing.^ 8 AlgAdd.- AlgRing.one) % (a AlgAdd.- AlgRing.one)) AlgRing.^ n  AlgAdd.+
        (a AlgAdd.+ AlgRing.one) :% a  AlgAdd.+
          (a AlgRing.^2 AlgAdd.+ 3 .^ a) :% (a AlgRing.^ 3 AlgAdd.- a AlgAdd.+ AlgRing.one)
  where
    a = qsoleParameter

g :: Integer -> RatioOfQSprays
g n = ((x^**^8 ^-^ unitSpray) %//% (x ^-^ unitSpray)) AlgRing.^ n  AlgAdd.+
        RatioOfSprays (x ^+^ unitSpray) x AlgAdd.+ 
          RatioOfSprays (x^**^2 ^+^ 3 .^ x) (x^**^3 ^-^ x ^+^ unitSpray)
  where
    x = qlone 1 

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

combn2 :: Int -> [(Int, (Int, Int))]
combn2 n = zip range (zip row1 row2)
  where
    range = [0 .. n-2]
    row1  = concat $ scanl1 (++) (map (: []) range) 
    row2  = concatMap (\i -> replicate i i) [1 .. n-1]

combn2' :: Int -> [(Int, (Int, Int))] -- winner
combn2' n = zip [0 .. n-2] (zip row1 row2)
  where
    range = [1 .. n-1]
    row1  = concatMap (\i -> [0 .. i-1]) range
    row2  = concatMap (\i -> replicate i i) range

main :: IO ()
main = 
  defaultMain
    [ bgroup "ratios"
      [ bench "f 2" $ whnf f 2
      , bench "g 2" $ whnf g 2
      , bench "f 5" $ whnf f 5
      , bench "g 5" $ whnf g 5
      ]
    ]
