module Main where
import           Approx                         ( assertApproxEqual )
import           Data.Ratio                     ( (%) )
import           Math.Algebra.Hspray            ( Spray,
                                                  (^+^),
                                                  (^-^),
                                                  (^*^),
                                                  (^**^),
                                                  (*^),
                                                  lone,
                                                  unitSpray,
                                                  zeroSpray,
                                                  constantSpray,
                                                  evalSpray,
                                                  substituteSpray,
                                                  composeSpray,
                                                  permuteVariables,
                                                  swapVariables,
                                                  fromList,
                                                  toList,
                                                  bombieriSpray,
                                                  derivSpray,
                                                  groebner,
                                                  fromRationalSpray,
                                                  esPolynomial,
                                                  isSymmetricSpray, 
                                                  isPolynomialOf,
                                                  resultant,
                                                  subresultants,
                                                  resultant1,
                                                  subresultants1
                                                )
import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( assertEqual
                                                , assertBool
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
      assertEqual "" p (fromList . toList $ p),

    testCase "derivSpray" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p1 = x ^+^ y ^*^ z ^**^ 3
        p2 = (x ^*^ y ^*^ z) ^+^ (2 *^ (x ^**^ 3 ^*^ y ^**^ 2))
        q = p1 ^*^ p2
        p1' = derivSpray 1 p1
        p2' = derivSpray 1 p2
        q'  = derivSpray 1 q
      assertEqual "" q' ((p1' ^*^ p2) ^+^ (p1 ^*^ p2')),

    testCase "groebner" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        p1 = x^**^2 ^+^ y ^+^ z ^-^ unitSpray
        p2 = x ^+^ y^**^2 ^+^ z ^-^ unitSpray
        p3 = x ^+^ y ^+^ z^**^2 ^-^ unitSpray
        g = groebner [p1, p2, p3] True
        xyz = [sqrt 2 - 1, sqrt 2 - 1, sqrt 2 - 1]
        gxyz = map ((`evalSpray` xyz) . fromRationalSpray) g
        sumAbsValues = sum $ map abs gxyz
      assertApproxEqual "" 8 sumAbsValues 0,

    testCase "symmetric polynomials" $ do
      let
        e2 = esPolynomial 4 2 :: Spray Rational
        e3 = esPolynomial 4 3 :: Spray Rational
        p = e2^**^2 ^+^ (2*^ e3)
      assertBool "" (isSymmetricSpray p),

    testCase "isPolynomialOf" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p1 = x ^+^ y
        p2 = x ^-^ y
        p = p1 ^*^ p2
      assertEqual "" (isPolynomialOf p [p1, p2]) (True, Just $ x ^*^ y),

    testCase "substituteSpray" $ do
      let
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = x1^**^2 ^+^ x2 ^+^ x3 ^-^ unitSpray
        p' = substituteSpray [Just 2, Nothing, Just 3] p
      assertEqual "" p' (x2 ^+^ (6*^ unitSpray)),

    testCase "permuteVariables" $ do
      let
        f :: Spray Rational -> Spray Rational -> Spray Rational -> Spray Rational
        f p1 p2 p3 = p1^**^4 ^+^ (2 *^ p2^**^3) ^+^ (3 *^ p3^**^2) ^-^ (4 *^ unitSpray)
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = f x1 x2 x3
        p' = permuteVariables p [3, 1, 2]
      assertEqual "" p' (f x3 x1 x2),

    testCase "swapVariables" $ do
      let
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = x1^**^4 ^+^ (2 *^ x2^**^3) ^+^ (3 *^ x3^**^2) ^-^ (4 *^ unitSpray)
        p' = permuteVariables p [3, 2, 1]
      assertEqual "" p' (swapVariables p (1, 3)),

    testCase "resultant" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
        q = x ^-^ (2*^ y^**^2)
        r = resultant 1 p q
      assertEqual "" r (x^**^4 ^-^ (8*^ x^**^6) ^+^ (16*^ x^**^8)),

    testCase "subresultants" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p = x^**^2 ^*^ y ^*^ (y^**^2 ^-^ 5*^ x ^+^ constantSpray 6) 
        q = x^**^2 ^*^ y ^*^ (3*^ y ^+^ constantSpray 2)
        sx = subresultants 1 p q
      assertBool "" (sx!!0 == zeroSpray && sx!!1 == zeroSpray && sx!!2 /= zeroSpray),

    testCase "resultant1" $ do
      let
        x = lone 1 :: Spray Rational
        p = x^**^2 ^-^ 5*^x ^+^ constantSpray 6 
        q = x^**^2 ^-^ 3*^x ^+^ constantSpray 2 
      assertEqual "" (resultant1 p q) (0%1),

    testCase "subresultants1" $ do
      let
        x = lone 1 :: Spray Rational
        p = x^**^2 ^-^ 5*^x ^+^ constantSpray 6 
        q = x^**^2 ^-^ 3*^x ^+^ constantSpray 2 
      assertEqual "" (subresultants1 p q) [0%1, 2%1, 1%1]

  ]
