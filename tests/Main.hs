module Main (main) where
import qualified Algebra.Additive               as AlgAdd
import qualified Algebra.Module                 as AlgMod
import qualified Algebra.Ring                   as AlgRing      
import qualified Algebra.Field                  as AlgField      
import           Approx                         ( approx, assertApproxEqual )
import           Data.Matrix                    ( Matrix, fromLists )
import           Data.Maybe                     ( fromJust )
import           Data.Ratio                     ( (%) )
import           Math.Algebra.Hspray            ( Spray,
                                                  QSpray,
                                                  (^+^),
                                                  (^-^),
                                                  (^*^),
                                                  (^**^),
                                                  (*^),
                                                  (.^),
                                                  (/>),
                                                  (/^),
                                                  lone,
                                                  qlone,
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
                                                  collinearSprays,
                                                  derivative,
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
                                                  gcdSpray,
                                                  QSpray',
                                                  Rational',
                                                  OneParameterQSpray,
                                                  evalRatioOfPolynomials,
                                                  evalOneParameterSpray',
                                                  qpolyFromCoeffs,
                                                  constQPoly,
                                                  evalOneParameterSpray'',
                                                  prettyQSpray,
                                                  prettyQSprayX1X2X3,
                                                  prettySpray,
                                                  prettySpray'',
                                                  qsoleParameter,
                                                  constQPoly,
                                                  prettyOneParameterQSpray',
                                                  (*.),
                                                  RatioOfSprays (..),
                                                  RatioOfQSprays,
                                                  (%//%),
                                                  (%/%),
                                                  (%:%),
                                                  unitRatioOfSprays,
                                                  isPolynomialRatioOfSprays,
                                                  evalRatioOfSprays,
                                                  substituteRatioOfSprays,
                                                  prettyRatioOfQSprays,
                                                  characteristicPolynomial,
                                                  detLaplace',
                                                  gegenbauerPolynomial,
                                                  jacobiPolynomial,
                                                  asRatioOfSprays,
                                                  SimpleParametricQSpray,
                                                  ParametricQSpray,
                                                  ParametricSpray,
                                                  zeroRatioOfSprays,
                                                  fromRatioOfQPolynomials,
                                                  (^/^),
                                                  HasVariables (..),
                                                  numberOfParameters,
                                                  changeParameters,
                                                  substituteParameters, 
                                                  evalParametricSpray,
                                                  asSimpleParametricSpray,
                                                  parametricSprayToOneParameterSpray
                                                )
import           MathObj.Matrix                 ( fromRows )
import qualified MathObj.Matrix                 as MathMatrix
import           Number.Ratio                   ( T ( (:%) ) )
import qualified Number.Ratio                   as NR
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

  [ 
    testCase "substituteParameters in Jacobi polynomial -> Legendre" $ do
      let 
        x = qlone 1
        jacobi   = jacobiPolynomial 5
        legendre = (63*^x^**^5 ^-^ 70*^x^**^3 ^+^ 15*^x) /^ 8 
      assertEqual "" legendre (substituteParameters jacobi [0, 0])

    , testCase "substituteParameters and evalParametricSpray" $ do
      let 
        jacobi  = jacobiPolynomial 5
        jacobi' = substituteParameters jacobi [3, 2%7]
        r1 = evaluate jacobi' [13]
        r2 = evaluate (evalParametricSpray jacobi [13]) [3, 2%7] 
      assertEqual "" r1 r2

    , testCase "changeParameters in Jacobi polynomial -> Gegenbauer" $ do
      let 
        risingFactorial :: QSpray -> Int -> QSpray
        risingFactorial theta n = 
          AlgRing.product 
            (map (\k -> theta ^+^ constantSpray (toRational k)) [0 .. n-1]) 
        m = 5
        alpha = qlone 1
        jacobi = 
          changeParameters (jacobiPolynomial m) 
            [alpha ^-^ constantSpray (1%2), alpha ^-^ constantSpray (1%2)]
        factor = 
          risingFactorial (2 *^ alpha) m %//% 
            risingFactorial (alpha ^+^ constantSpray (1%2)) m
        obtained = asSimpleParametricSpray (factor *^ jacobi)
        gegenbauer = gegenbauerPolynomial m
      assertEqual "" gegenbauer obtained

    , testCase "changeParameters in Jacobi polynomial" $ do
      let 
        n = 5
        jp_n       = jacobiPolynomial n
        jp_nminus1 = jacobiPolynomial (n-1)
        jp_n'      = derivative 1 jp_n
        a = qlone 1
        b = qlone 2
        spray = (a ^+^ b ^+^ constantSpray (toRational $ n + 1)) /^ 2
        rOS = asRatioOfSprays spray
        rhs = rOS *^ 
          changeParameters jp_nminus1 [a ^+^ unitSpray, b ^+^ unitSpray]
      assertEqual "" jp_n' rhs

    , testCase "numberOfParameters in Jacobi polynomial" $ do
      let 
        jp = jacobiPolynomial 5
      assertEqual "" (numberOfParameters jp) 2

    , testCase "changeVariables in ratioOfSprays" $ do
      let
        f :: QSpray -> QSpray -> RatioOfQSprays
        f p1 p2 = (p1^**^2 ^+^ 2 *^ p2) %//% (p1^**^3 ^-^ unitSpray)
        x = qlone 1
        y = qlone 2
        rOS = f x y
        u = x ^*^ y
        v = x^**^2 ^-^ y ^+^ unitSpray 
        rOS' = f u v 
      assertEqual "" rOS' (changeVariables rOS [u, v])

    , testCase "fromRatioOfQPolynomials" $ do
      let
        a = qsoleParameter
        x = qlone 1
        rOP = ((a AlgRing.^ 8 AlgAdd.- AlgRing.one) ^/^ 
                                (a AlgAdd.- AlgRing.one)) AlgRing.^ 3
              AlgAdd.+ (a AlgAdd.+ AlgRing.one) :% a 
        rOQ = ((x^**^8 ^-^ unitSpray) %//% (x ^-^ unitSpray)) AlgRing.^ 3 
              AlgAdd.+ RatioOfSprays (x ^+^ unitSpray) x
      assertEqual "" rOQ (fromRatioOfQPolynomials rOP)

    , testCase "(.^)" $ do
      let
        x = lone 1 :: QSpray
        y = lone 2 :: QSpray
        rOS = (x^**^4 ^-^ y^**^4) %//% (x ^+^ y ^-^ unitSpray)
      assertEqual "" (10 .^ rOS AlgAdd.+ (-10) .^ rOS) zeroRatioOfSprays 

    , testCase "module `ParametricSpray a` over `a`" $ do
      let
        x = lone 1 :: ParametricQSpray
        y = lone 2 :: ParametricQSpray
        p = x^**^2 ^+^ x^*^y ^-^ unitSpray
        lambda = 3 :: Rational
        p' = asRatioOfSprays (lambda *^ unitSpray) *^ p
      assertEqual "" (lambda AlgMod.*> p) p'

    , testCase "Jacobi polynomial" $ do 
      let
        jp2 = jacobiPolynomial 2
        alpha0 = qlone 1
        beta0  = qlone 2
        cst :: Rational -> QSpray
        cst = constantSpray
        x = lone 1 :: ParametricQSpray
        p = x ^-^ unitSpray
        t1 = asRatioOfSprays (((alpha0 ^+^ cst 1)^*^(alpha0 ^+^ cst 2)) /^ 2)
        t2 = asRatioOfSprays (((alpha0 ^+^ cst 2)^*^(alpha0 ^+^ beta0 ^+^ cst 3)) /^ 2) 
        t3 = asRatioOfSprays (((alpha0 ^+^ beta0 ^+^ cst 3)^*^(alpha0 ^+^ beta0 ^+^ cst 4)) /^ 8) 
        expected = t1 *^ unitSpray  ^+^  t2 *^ p  ^+^  t3 *^ p^**^2 
      assertEqual "" jp2 expected

    , testCase "characteristic polynomial" $ do
      let
        m = fromLists [ [12, 16, 4]
                      , [16, 2, 8]
                      , [8, 18, 10] ] :: Matrix Int
        spray = characteristicPolynomial m
        x = lone 1 :: Spray Int
        expected = 
          AlgAdd.negate x^**^3 ^+^ 24*^x^**^2 ^+^ 268*^x ^-^ constantSpray 1936
      assertEqual "" spray expected

    , testCase "determinant of product" $ do
      let
        m = fromRows 3 3 [ [12, 16, 4]
                         , [16, 2, 8]
                         , [8, 18, 10] ] :: MathMatrix.T Int
      assertEqual "" (detLaplace' (m AlgRing.* m)) (AlgRing.sqr (detLaplace' m))

    , testCase "ratio of sprays is irreducible" $ do
      let
        x = lone 1 :: QSpray
        y = lone 2 :: QSpray
        w = lone 4 :: QSpray
        rOS = (x^**^4 ^-^ y^**^4) %//% (x ^-^ y)
        rOS' = rOS %/% (x^**^4 ^-^ y^**^4)
        rOS'' = w AlgMod.*> rOS'
      assertEqual "" 
        (
            prettyRatioOfQSprays rOS
          , prettyRatioOfQSprays rOS'
          , prettyRatioOfQSprays rOS''
        ) 
        (
          "[ x^3 + x^2.y + x.y^2 + y^3 ]"
        , "[ 1 ] %//% [ x - y ]"
        , "[ x4 ] %//% [ x1 - x2 ]"
        )

    , testCase "isPolynomialRatioOfSprays" $ do
      let
        x = qlone 1
        y = qlone 2
        rOS = (x^**^4 ^-^ y^**^4) %//% (x ^-^ y)
      assertBool "" (isPolynomialRatioOfSprays rOS)

    , testCase "power of ratio of sprays" $ do
      let
        x = qlone 1
        y = qlone 2
        z = qlone 3
        p = x^**^4  ^-^  x ^*^ y^**^4  ^+^ x ^*^ z
        q = x  ^-^  x ^*^ y
        rOS = p %//% q
        rOS' = p^**^4 %//% q^**^4
      assertEqual "" rOS' (rOS AlgRing.^ 4)

    , testCase "arithmetic on ratio of sprays" $ do
      let
        x = qlone 1  
        y = qlone 2 
        p = x^**^2 ^-^ 3*^(x ^*^ y) ^+^ y^**^3 
        q = x ^-^ y
        rOS1 = p^**^2 %//% q
        rOS2 = rOS1 AlgAdd.+ unitRatioOfSprays
        rOS = rOS1 AlgRing.^ 2 AlgAdd.+ rOS1 AlgRing.* rOS2 AlgAdd.- rOS1
        test1 = 
          (rOS1 AlgAdd.+ rOS2) AlgRing.* (rOS1 AlgAdd.- rOS2) == 
            rOS1 AlgRing.^ 2 AlgAdd.- rOS2 AlgRing.^ 2
        rOS' = (3%4 :: Rational) AlgMod.*> rOS AlgRing.^ 2 AlgAdd.+ p AlgMod.*> rOS
        test2 = p AlgMod.*> (rOS' %/% p) == rOS'
        test3 = rOS1 %/% p == p %//% q
        test4 = rOS' AlgField./ rOS' == unitRatioOfSprays
        k = 3 :: Rational
        test5 = (p /> k) AlgMod.*> rOS1 == p AlgMod.*> (rOS1 /> k)
      assertEqual "" [test1, test2, test3, test4, test5] [True, True, True, True, True]

    , testCase "evaluate ratio of sprays" $ do
      let
        x = qlone 1
        y = qlone 2
        p = x ^+^ y
        q = x ^-^ y
        rOS1 = p %//% q
        rOS2 = q %//% p
        f :: AlgField.C a => a -> a -> a
        f u v = u AlgRing.^ 2  AlgAdd.+  u AlgRing.* v  AlgAdd.-  u AlgField./ v
        rOS = f rOS1 rOS2
        values = [2%3, 7%4]
        r1 = evalRatioOfSprays rOS1 values
        r2 = evalRatioOfSprays rOS2 values
        r = evalRatioOfSprays rOS values
      assertEqual "" r (f r1 r2)

    , testCase "Gegenbauer: differential equation and Chebyshev case" $ do
      let
        n = 5
        g   = gegenbauerPolynomial n
        g'  = derivative 1 g
        g'' = derivative 1 g'
        alpha = lone 1 :: QSpray
        x     = lone 1 :: SimpleParametricQSpray
        nAsSpray = constantSpray (toRational n)
        shouldBeZero = 
          (unitSpray ^-^ x^**^2) ^*^ g''
            ^-^ (2.^alpha ^+^ unitSpray) *^ (x ^*^ g')
              ^+^ n.^(nAsSpray ^+^ 2.^alpha) *^ g
        chebyshev = fromRationalSpray $ substituteParameters g [1]
        theta = 2.5
      assertEqual "" 
        (shouldBeZero, approx 8 $ sin theta * evalSpray chebyshev [cos theta]) 
        (zeroSpray, approx 8 $ sin (fromIntegral (n+1) * theta))

    , testCase "scale spray by integer" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p = 2 *^ (2 *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ 4 *^ z ^+^ 5 *^ unitSpray
        spray = p ^+^ p ^+^ p ^+^ p ^+^ p ^+^ p ^+^ p
      assertEqual "" (7 .^ p, (-7) .^ p, 0 .^ p) (spray, AlgAdd.negate spray, zeroSpray)

    , testCase "collinearSprays" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        spray1 =
          (2 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ (4 % 1) *^ z ^+^ (5 % 1) *^ unitSpray
        spray2 = 121 *^ spray1
      assertBool "" (collinearSprays spray1 spray2)
    
    , testCase "bombieriSpray" $ do
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
      assertEqual "" p (fromList . toList $ p)

    , testCase "derivative of spray" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p1 = x ^+^ y ^*^ z ^**^ 3
        p2 = (x ^*^ y ^*^ z) ^+^ (2 *^ (x ^**^ 3 ^*^ y ^**^ 2))
        q = p1 ^*^ p2
        p1' = derivative 1 p1
        p2' = derivative 1 p2
        q'  = derivative 1 q
      assertEqual "" q' ((p1' ^*^ p2) ^+^ (p1 ^*^ p2'))

    , testCase "derivative of a ratio of sprays" $ do
      let
        x = lone 1 :: QSpray
        y = lone 2 :: QSpray
        rOS = x %//% y
        rOS' = derivative 2 rOS
        expected = AlgAdd.negate x %//% y^**^2 
      assertEqual "" rOS' expected

    , testCase "groebner" $ do
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

    testCase "isPolynomialOf - 2" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
      assertEqual "" 
        (isPolynomialOf x [x ^+^ y^*^z, y, z]) 
        (True, Just $ x ^-^ y^*^z),

    testCase "power sum polynomials" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        symSpray = x^**^2 ^+^ y^**^2 ^+^ x ^+^ y
        p1 = psPolynomial 2 1 :: Spray Rational
        p2 = psPolynomial 2 2 :: Spray Rational
        p = fromJust $ snd $ isPolynomialOf symSpray [p1, p2]
        symSpray' = composeSpray p [p1, p2]
      assertEqual "" symSpray symSpray'

    , testCase "substituteSpray" $ do
      let
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = x1^**^2 ^+^ x2 ^+^ x3 ^-^ unitSpray
        p' = substituteSpray [Just 2, Nothing, Just 3] p
      assertEqual "" p' (x2 ^+^ (6*^ unitSpray))

    , testCase "substituteRatioOfSprays" $ do
      let
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = x1^**^2 ^+^ x2 ^+^ x3 ^-^ unitSpray
        q = x1 ^-^ x2
        rOS = p %//% q
        subs = [Just 2, Nothing, Just 3]
        p' = substituteSpray subs p
        q' = substituteSpray subs q
        rOS' = substituteRatioOfSprays subs rOS
      assertEqual "" rOS' (p' %//% q')

    , testCase "permuteVariables of a spray" $ do
      let
        f :: Spray Rational -> Spray Rational -> Spray Rational -> Spray Rational
        f p1 p2 p3 = p1^**^4 ^+^ (2 *^ p2^**^3) ^+^ (3 *^ p3^**^2) ^-^ (4 *^ unitSpray)
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = f x1 x2 x3
        p' = permuteVariables [3, 1, 2] p
      assertEqual "" p' (f x3 x1 x2)

    , testCase "swapVariables of a spray" $ do
      let
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = x1^**^4 ^+^ (2 *^ x2^**^3) ^+^ (3 *^ x3^**^2) ^-^ (4 *^ unitSpray)
        p' = permuteVariables [3, 2, 1] p
      assertEqual "" p' (swapVariables (1, 3) p)

    , testCase "permuteVariables of a ratio of sprays" $ do
      let
        f :: QSpray -> QSpray -> QSpray -> RatioOfQSprays
        f p1 p2 p3 = (p1^**^4 ^+^ (2 *^ p2^**^3)) %//% ((3 *^ p3^**^2) ^-^ (4 *^ unitSpray))
        x1 = lone 1 :: QSpray
        x2 = lone 2 :: QSpray
        x3 = lone 3 :: QSpray
        rOS = f x1 x2 x3
        rOS' = permuteVariables [3, 1, 2] rOS
      assertEqual "" rOS' (f x3 x1 x2)

    , testCase "swapVariables of a ratio of sprays" $ do
      let
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        rOS = (x1^**^4 ^+^ (2 *^ x2^**^3)) %//% ((3 *^ x3^**^2) ^-^ (4 *^ unitSpray))
        rOS' = permuteVariables [3, 2, 1] rOS
      assertEqual "" rOS' (swapVariables (1, 3) rOS)

    , testCase "resultant w.r.t x" $ do
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
      assertBool "" (gcdSpray sprayA sprayB1 == unitSpray && gcdSpray sprayA sprayB2 == unitSpray)

    , testCase "sprayDivision" $ do
      let 
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        sprayB = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  constantSpray 3
        sprayQ = x^**^4  ^-^  x  ^+^  y^**^2
        sprayA = sprayB ^*^ sprayQ 
      assertEqual "" (sprayDivision sprayA sprayB) (sprayQ, zeroSpray)

    , testCase "sprayDivision by constant spray" $ do
      let 
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        sprayA = 3*^(x^**^2 ^*^ y)  ^-^  3*^(x ^*^ y)  ^+^  constantSpray 3
        sprayB = constantSpray 3
        expected = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  unitSpray
      assertEqual "" (sprayDivision sprayA sprayB) (expected, zeroSpray),

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
      assertEqual "" g sprayD,

    testCase "evaluation of symbolic spray" $ do
      let 
        a    = qpolyFromCoeffs [0, 1]  
        p    = a AlgRing.^ 2 AlgAdd.- constQPoly 4 
        q1   = a AlgAdd.- constQPoly 3
        q2   = a AlgAdd.- constQPoly 2
        rop1 = p :% q1 
        rop2 = p :% q2
        f :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> Spray a -> (Spray a, Spray a)
        f x y z = (x^**^2 ^+^ y^**^2, z)
        g :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> Spray a -> (a, a, a) -> (a, a)
        g px py pz (x, y, z) = (evalSpray f1 [x, y, AlgAdd.zero], evalSpray f2 [AlgAdd.zero, AlgAdd.zero, z])
          where (f1, f2) = f px py pz
        (r1, r2) = g (lone 1 :: QSpray') (lone 2) (lone 3) (2, 3, 4) 
        r = evalRatioOfPolynomials 5 rop1 AlgRing.* r1  AlgAdd.+  evalRatioOfPolynomials 5 rop2 AlgRing.* r2
        (f1', f2')  = f (lone 1 :: OneParameterQSpray) (lone 2) (lone 3)
        symSpray  = rop1 *^ f1'  ^+^  rop2 *^ f2' 
        r' = evalOneParameterSpray' symSpray 5 [2, 3, 4]
        rop1' = evalOneParameterSpray'' f1' [2, 3]
        rop2' = evalOneParameterSpray'' f2' [0, 0, 4]
        r'' = evalRatioOfPolynomials 5 (rop1 AlgRing.* rop1' AlgAdd.+ rop2 AlgRing.* rop2')
      assertEqual "" (r, r') (r', r''),

    testCase "pretty spray" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        p1 = ((2%3) *^ x^**^3) ^*^ y  ^-^  x^**^2  ^+^  y ^*^ z  ^-^  (2%3) *^ unitSpray
        p2 = (3%2) *^ p1
        p3 = AlgAdd.negate $ 
          swapVariables (1, 3) $ 
            p2  ^+^  unitSpray  ^-^  (x^**^3 ^*^ y  ^-^  ((3%2) *^ x^**^2))
        strings = 
          [
            prettyQSpray (zeroSpray ^*^ p1)
          , prettyQSpray p1
          , prettyQSpray (AlgAdd.negate p2)
          , prettyQSpray (p2  ^+^  lone 4)
          , prettyQSpray p3
          , " ---------- "
          , prettyQSprayX1X2X3 "a" (zeroSpray ^*^ p1)
          , prettyQSprayX1X2X3 "a" p1
          , prettyQSprayX1X2X3 "a" (p2  ^+^  lone 4)
          , prettyQSprayX1X2X3 "a" p3
          , " ---------- "
          , prettySpray (zeroSpray ^*^ p1)
          , prettySpray p1
          , prettySpray (p2  ^+^  lone 4)
          , prettySpray p3
          , " ---------- "
          , prettySpray'' "w" (zeroSpray ^*^ p1)
          , prettySpray'' "w" p1
          , prettySpray'' "w" (p2  ^+^  lone 4)
          , prettySpray'' "w" p3
          ]
        strings' =
          [
            "0"
          , "(2/3)*x^3.y - x^2 + y.z - (2/3)"
          , "-x^3.y + (3/2)*x^2 - (3/2)*y.z + 1"
          , "x1^3.x2 - (3/2)*x1^2 + (3/2)*x2.x3 + x4 - 1"
          , "-(3/2)*x.y"
          , " ---------- "
          , "0"
          , "(2/3)*a1^3.a2 - a1^2 + a2.a3 - (2/3)"
          , "a1^3.a2 - (3/2)*a1^2 + (3/2)*a2.a3 + a4 - 1"
          , "-(3/2)*a1.a2"
          , " ---------- "
          , "0"
          , "(2 % 3)*x^3.y + ((-1) % 1)*x^2 + (1 % 1)*y.z + ((-2) % 3)"
          , "(1 % 1)*x1^3.x2 + ((-3) % 2)*x1^2 + (3 % 2)*x2.x3 + (1 % 1)*x4 + ((-1) % 1)"
          , "((-3) % 2)*x.y"
          , " ---------- "
          , "0"
          , "(2 % 3)*w^(3, 1) + ((-1) % 1)*w^(2) + (1 % 1)*w^(0, 1, 1) + ((-2) % 3)*w^()"
          , "(1 % 1)*w^(3, 1) + ((-3) % 2)*w^(2) + (3 % 2)*w^(0, 1, 1) + (1 % 1)*w^(0, 0, 0, 1) + ((-1) % 1)*w^()"
          , "((-3) % 2)*w^(1, 1)"  
          ]
      assertEqual "" strings strings',

    testCase "prettyOneParameterQSpray'" $ do
      let
        x = lone 1 :: OneParameterQSpray 
        y = lone 2 :: OneParameterQSpray 
        z = lone 3 :: OneParameterQSpray 
        a = qsoleParameter  
        sSpray 
          = ((4 NR.% 5) *. (a :% (a AlgRing.^ 2 AlgAdd.+ AlgRing.one))) AlgMod.*> (x^**^2 ^-^ y^**^2)  
            ^+^  (constQPoly (2 NR.% 3) AlgRing.* a) AlgMod.*> (y ^*^ z)
        string = prettyOneParameterQSpray' "a" sSpray
        string' = 
          "{ [ (4/5)*a ] %//% [ a^2 + 1 ] }*X^2 + { [ -(4/5)*a ] %//% [ a^2 + 1 ] }*Y^2 + { (2/3)*a }*Y.Z"
      assertEqual "" string string'

    , testCase "parametricSprayToOneParameterSpray" $ do
      let 
        x = lone 1 :: OneParameterQSpray 
        y = lone 2 :: OneParameterQSpray 
        z = lone 3 :: OneParameterQSpray 
        a = qsoleParameter  
        sSpray 
          = ((4 NR.% 5) *. (a :% (a AlgRing.^ 2 AlgAdd.+ AlgRing.one))) AlgMod.*> (x^**^2 ^-^ y^**^2)  
            ^+^  (constQPoly (2 NR.% 3) AlgRing.* a) AlgMod.*> (y ^*^ z)
        x' = lone 1 :: ParametricSpray Rational'
        y' = lone 2 :: ParametricSpray Rational'
        z' = lone 3 :: ParametricSpray Rational'
        a' = lone 1 :: Spray Rational'
        spray  
          = ((4 NR.% 5 :: Rational') AlgMod.*> (a' %:% (a'^**^2 ^+^ unitSpray))) *^ (x'^**^2 ^-^ y'^**^2)  
            ^+^  ((2 NR.% 3) *^ a') AlgMod.*> (y' ^*^ z')
      assertEqual "" (parametricSprayToOneParameterSpray spray) sSpray


  ]
