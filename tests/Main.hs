module Main where
import           Data.Ratio
import           Math.Algebra.Hspray
import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( assertEqual
                                                , testCase
                                                )

main :: IO ()
main = defaultMain $ testGroup
  "Tests"
  [ testCase "bombieriSpray" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        poly =
          (2 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ (4 % 1) *^ z ^+^ (5 % 1) *^ unitSpray
        bpoly =
          (24 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ (4 % 1) *^ z ^+^ (5 % 1) *^ unitSpray
      assertEqual "" bpoly (bombieriSpray poly)
  ]
