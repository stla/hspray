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
                                                  getCoefficient,
                                                  getConstantTerm,
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
                                                  psPolynomial, 
                                                  isSymmetricSpray, 
                                                  isPolynomialOf,
                                                  resultant,
                                                  resultant',
                                                  subresultants,
                                                  resultant1,
                                                  subresultants1,
                                                  sprayDivision,
                                                  gcdSpray
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

    testCase "getCoefficient" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p = 2 *^ (2 *^ (x^**^3 ^*^ y^**^2)) ^+^ 4 *^ z ^+^ 5 *^ unitSpray
      assertEqual "" (getCoefficient [3, 2, 0] p, getCoefficient [0, 4] p) (4, 0),

    testCase "getConstantTerm" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p = 2 *^ (2 *^ (x^**^3 ^*^ y^**^2)) ^+^ 4 *^ z ^+^ 5 *^ unitSpray
      assertEqual "" (getConstantTerm p) 5,

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

    testCase "symmetric polynomial" $ do
      let
        e2 = esPolynomial 4 2 :: Spray Rational
        e3 = esPolynomial 4 3 :: Spray Rational
        p = e2^**^2 ^+^ (2*^ e3)
      assertBool "" (isSymmetricSpray p),

    testCase "Schur polynomial is symmetric" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        p =  x^**^3 ^*^ y^**^2 ^*^ z ^+^ x^**^3 ^*^ y ^*^ z^**^2 ^+^ x^**^2 ^*^ y^**^3 ^*^ z ^+^ 2*^(x^**^2 ^*^ y^**^2 ^*^ z^**^2) ^+^ x^**^2 ^*^ y ^*^ z^**^3 ^+^ x ^*^ y^**^3 ^*^ z^**^2 ^+^ x ^*^ y^**^2 ^*^ z^**^3
      assertBool "" (isSymmetricSpray p),

    testCase "isPolynomialOf" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p1 = x ^+^ y
        p2 = x ^-^ y
        p = p1 ^*^ p2 ^+^ unitSpray
      assertEqual "" (isPolynomialOf p [p1, p2]) (True, Just $ x ^*^ y ^+^ unitSpray),

    testCase "power sum polynomials" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        symSpray = x^**^2 ^+^ y^**^2 ^+^ x ^+^ y
        p1 = psPolynomial 2 1 :: Spray Rational
        p2 = psPolynomial 2 2 :: Spray Rational
        (_, Just p) = isPolynomialOf symSpray [p1, p2]
        symSpray' = composeSpray p [p1, p2]
      assertEqual "" symSpray symSpray',

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
        p' = permuteVariables [3, 1, 2] p
      assertEqual "" p' (f x3 x1 x2),

    testCase "swapVariables" $ do
      let
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = x1^**^4 ^+^ (2 *^ x2^**^3) ^+^ (3 *^ x3^**^2) ^-^ (4 *^ unitSpray)
        p' = permuteVariables [3, 2, 1] p
      assertEqual "" p' (swapVariables (1, 3) p),

    testCase "resultant w.r.t x" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
        q = x ^-^ (2*^ y^**^2)
        r = resultant 1 p q
      assertEqual "" r (y^**^4 ^-^ (8*^ y^**^6) ^+^ (16*^ y^**^8)),

    testCase "resultant w.r.t y" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
        q = x ^-^ (2*^ y^**^2)
        r = resultant 2 p q
      assertEqual "" r (16*^x^**^8 ^-^ 32*^x^**^7 ^+^ 24*^x^**^6 ^-^ 8*^x^**^5 ^+^ x^**^4),

    testCase "resultant product rule" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        f = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
        g = x ^-^ (2*^ y^**^2)
        h = x^**^2 ^*^ y ^+^ y^**^3 ^+^ unitSpray
      assertEqual "" (resultant 1 (f^*^g) h) (resultant 1 f h ^*^ resultant 1 g h),

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

    testCase "resultant1 product rule" $ do
      let
        x = lone 1 :: Spray Rational
        f = x^**^2 ^-^ 5*^x ^+^ constantSpray 6 
        g = x^**^2 ^-^ 3*^x ^+^ constantSpray 2 
        h = x^**^3 ^+^ x ^-^ constantSpray 3
      assertEqual "" (resultant1 (f^*^g) h) (resultant1 f h * resultant1 g h),

    testCase "subresultants1" $ do
      let
        x = lone 1 :: Spray Rational
        p = x^**^2 ^-^ 5*^x ^+^ constantSpray 6 
        q = x^**^2 ^-^ 3*^x ^+^ constantSpray 2 
      assertEqual "" (subresultants1 p q) [0%1, 2%1, 1%1],

    testCase "resultant agrees with resultant1 for univariate case" $ do
      let
        x = lone 1 :: Spray Rational
        f = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^x 
        g = x ^-^ (2*^ x^**^2) ^+^ constantSpray 4
        r = resultant 1 f g
        r1 = resultant1 f g
      assertEqual "" r1 (getCoefficient [] r),

    testCase "resultant and resultant' are in agreement" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        p = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ z^**^4 
        q = x ^-^ (2*^ y^**^2) ^*^ z^**^2 ^*^ unitSpray
        rx  = resultant 1 p q
        rx' = resultant' 1 p q
        ry  = resultant 2 p q
        ry' = resultant' 2 p q
        rz  = resultant 3 p q
        rz' = resultant' 3 p q
        test1 = rx == rx'
        test2 = ry == ry'
        test3 = rz == rz'
      assertBool "" (test1 && test2 && test3),

    testCase "gcdSpray - univariate example" $ do
      let
        x = lone 1 :: Spray Rational
        sprayD = x^**^2 ^+^ unitSpray
        sprayA = sprayD ^*^ (x^**^4 ^-^ x) 
        sprayB = sprayD ^*^ (2*^x ^+^ unitSpray)
        sprayGCD = gcdSpray sprayA sprayB
      assertEqual "" sprayGCD (9 *^ sprayD),

    testCase "gcdSpray with a constant spray" $ do
      let
        x = lone 1 :: Spray Rational
        sprayA = 3 *^ x^**^4 ^-^ x 
        b1 = 2 :: Rational
        b2 = 4 :: Rational
        sprayB1 = constantSpray b1
        sprayB2 = constantSpray b2
      assertBool "" (gcdSpray sprayA sprayB1 == unitSpray && gcdSpray sprayA sprayB2 == unitSpray),

    testCase "sprayDivision" $ do
      let 
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        sprayB = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  constantSpray 3
        sprayQ = x^**^4  ^-^  x  ^+^  y^**^2
        sprayA = sprayB ^*^ sprayQ 
      assertEqual "" (sprayDivision sprayA sprayB) (sprayQ, zeroSpray),

    testCase "gcdSpray - bivariate example" $ do
      let 
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        sprayD = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  constantSpray 3
        sprayA = sprayD ^*^ (x^**^4  ^-^  x  ^+^  y^**^2) 
        sprayB = sprayD ^*^ y ^*^ (2*^x  ^+^  unitSpray)
        g = gcdSpray sprayA sprayB
      assertEqual "" g ((1%3) *^ sprayD),

    testCase "gcdSpray - trivariate example" $ do
      let 
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        sprayD = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  z  ^+^  constantSpray 3
        sprayA = sprayD^**^1 ^*^ (x^**^4  ^-^  x  ^+^  y   ^+^  x ^*^ y ^*^ z^**^2)
        sprayB = sprayD^**^1 ^*^ y ^*^ (2*^x  ^+^  unitSpray) ^*^ z
        g = gcdSpray sprayA sprayB
      assertEqual "" g sprayD

  ]
