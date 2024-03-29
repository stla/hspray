module Main where
import Test.Tasty.Bench ( bench, bgroup, defaultMain, nf )

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
    [ bgroup "combn2"
      [ bench "combn2 - 20"   $ nf combn2 20
      , bench "combn2' - 20"  $ nf combn2' 20
      , bench "combn2 - 100"  $ nf combn2 100
      , bench "combn2' - 100" $ nf combn2' 100
      ]
    ]
