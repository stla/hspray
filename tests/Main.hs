module Main where
import           Data.Ratio                     ( (%) )
import           Math.Algebra.Hspray            ( Spray,
                                                  (^+^),
                                                  (^*^),
                                                  (^**^),
                                                  (*^),
                                                  lone,
                                                  unitSpray,
                                                  evalSpray,
                                                  composeSpray,
                                                  fromList,
                                                  toList,
                                                  bombieriSpray )
import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( assertEqual
                                                , testCase
                                                )

main :: IO ()
main = defaultMain $ testGroup
  "Testing hspray"

  [ testCase "bombieriSpray" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        poly =
          (2 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ (4 % 1) *^ z ^+^ (5 % 1) *^ unitSpray
        bpoly =
          (24 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ (4 % 1) *^ z ^+^ (5 % 1) *^ unitSpray
      assertEqual "" bpoly (bombieriSpray poly),

    testCase "composeSpray" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p = 2 *^ (2 *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ 4 *^ z ^+^ 5 *^ unitSpray
        px = x ^+^ y ^+^ z
        py = x ^*^ y ^*^ z
        pz = y ^**^ 2
        q = composeSpray p [px, py, pz]
        xyz = [2, 3, 4]
        pxyz = map (`evalSpray` xyz) [px, py, pz]
      assertEqual "" (evalSpray p pxyz) (evalSpray q xyz),

    testCase "fromList . toList = identity" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p = 2 *^ (2 *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ 4 *^ z ^+^ 5 *^ unitSpray
      assertEqual "" p (fromList . toList $ p)

  ]
