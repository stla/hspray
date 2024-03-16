module Main where
import Test.Tasty.Bench ( bench, bgroup, defaultMain, nf )

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = 
  defaultMain
    [ bgroup "Fibonacci numbers"
      [ bench "fifth"     $ nf fibo  5
      , bench "tenth"     $ nf fibo 10
      , bench "twentieth" $ nf fibo 20
      ]
    ]
