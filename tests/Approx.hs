module Approx (approx, assertApproxEqual) where
import           Test.Tasty.HUnit ( Assertion, assertEqual )

-- round x to n digits
approx :: Int -> Double -> Double
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

assertApproxEqual :: String -> Int -> Double -> Double -> Assertion
assertApproxEqual prefix n x1 x2 = 
  assertEqual prefix (approx n x1) (approx n x2)
