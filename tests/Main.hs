{-# LANGUAGE TupleSections #-}
module Main (main) where
import qualified Algebra.Additive               as AlgAdd
import qualified Algebra.Module                 as AlgMod
import qualified Algebra.Ring                   as AlgRing      
import qualified Algebra.Field                  as AlgField      
import           Approx                         ( approx, assertApproxEqual )
import qualified Data.HashMap.Strict            as HM
import           Data.List                      ( sortOn )
import           Data.Matrix                    ( Matrix, fromLists )
import           Data.Maybe                     ( fromJust )
import           Data.Ratio                     ( (%) )
import           Math.Algebra.Hspray            ( Spray,
                                                  QSpray,
                                                  FunctionLike (..),
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
                                                  groebnerBasis,
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
                                                  polynomialSubresultants,
                                                  sturmHabichtSequence,
                                                  principalSturmHabichtSequence,
                                                  numberOfRealRootsInOpenInterval,
                                                  numberOfRealRootsInClosedInterval,
                                                  numberOfRealRoots, 
                                                  sprayDivision,
                                                  gcdSpray,
                                                  QSpray',
                                                  Rational',
                                                  Polynomial,
                                                  OneParameterQSpray,
                                                  evalRatioOfPolynomials,
                                                  evalOneParameterSpray',
                                                  constQPoly,
                                                  evalOneParameterSpray'',
                                                  prettyQSpray,
                                                  prettyQSprayX1X2X3,
                                                  prettySpray,
                                                  prettySpray'',
                                                  qsoleParameter,
                                                  soleParameter,
                                                  constQPoly,
                                                  constPoly,
                                                  prettyOneParameterQSpray',
                                                  RatioOfSprays (..),
                                                  RatioOfQSprays,
                                                  (%//%),
                                                  (%/%),
                                                  (%:%),
                                                  unitRatioOfSprays,
                                                  constantRatioOfSprays,
                                                  isPolynomialRatioOfSprays,
                                                  evalRatioOfSprays,
                                                  evalRatioOfSprays',
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
                                                  numberOfParameters,
                                                  changeParameters,
                                                  substituteParameters,
                                                  evalParametricSpray,
                                                  asSimpleParametricSpray,
                                                  parametricSprayToOneParameterSpray,
                                                  prettyParametricQSprayABCXYZ,
                                                  asSimpleParametricSpray,
                                                  constPoly,
                                                  qlone',
                                                  qmonomial,
                                                  isHomogeneousSpray,
                                                  psPolynomial,
                                                  prettyQSprayXYZ,
                                                  productOfSprays
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

type PQS = ParametricQSpray

main :: IO ()
main = defaultMain $ testGroup

  "Testing hspray"

  [ 
    testCase "involvesVariable" $ do 
      let
        x = qlone 1
        z = qlone 3
        p = x^**^4 ^-^ x ^+^ x ^*^ z
        tests = map (involvesVariable p) [0 .. 4]
      assertEqual "" tests [False, True, False, True, False]

    , testCase "dropVariables" $ do 
      let
        x = qlone 1
        y = qlone 2
        z = qlone 3
        p = y^**^4 ^-^ y ^+^ y ^*^ z
        p' = x^**^4 ^-^ x ^+^ x ^*^ y
      assertEqual "" (dropVariables 1 p) p'

    , testCase "power sum polynomials are homogeneous " $ do
      let
        lambda = [4, 3, 2, 2]
        sprays = map (psPolynomial 4) lambda :: [Spray Int]
        spray =  AlgRing.product sprays
        homogeneous = isHomogeneousSpray spray
      assertEqual "" homogeneous (True, Just $ sum lambda)   
    
    , testCase "qmonomial and qlone'" $ do
      let
        assocs = [(1, 5), (3, 7), (1, 5)]
        assocs0 = [(0, 4), (0, 0)]
      assertEqual "" 
        (qmonomial assocs,          qmonomial assocs0) 
        (qlone' 1 5 ^*^ qlone' 3 7, unitSpray)

    , testCase "asSimpleParametricSpray" $ do
      let
        jp = jacobiPolynomial 8
        jp' = asSimpleParametricSpray jp
        jp'' = HM.map asRatioOfSprays jp'
      assertEqual "" jp jp''

    , testCase "prettyParametricQSprayABCXYZ" $ do
      let
        f :: (QSpray, QSpray) -> (PQS, PQS, PQS) -> PQS
        f (a, b) (x, y, z) = 
          (a %:% (a ^+^ unitSpray)) *^ x^**^2  ^+^  
                  (b %:% (a ^+^ b)) *^ (y ^*^ z)
        pqs = f (lone 1, lone 2) (lone 1, lone 2, lone 3)
        s1 = prettyParametricQSprayABCXYZ ["a","b"] ["X","Y","Z"] pqs
        s2 = prettyParametricQSprayABCXYZ ["a"] ["X","Y","Z"] pqs
        s3 = prettyParametricQSprayABCXYZ ["a","b"] ["X","Y"] pqs
      assertEqual ""
        [
          s1, s2, s3
        ]
        [
          "{ [ a ] %//% [ a + 1 ] }*X^2 + { [ b ] %//% [ a + b ] }*Y.Z",
          "{ [ a1 ] %//% [ a1 + 1 ] }*X^2 + { [ a2 ] %//% [ a1 + a2 ] }*Y.Z",
          "{ [ a ] %//% [ a + 1 ] }*X1^2 + { [ b ] %//% [ a + b ] }*X2.X3"
        ]

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

    , testCase "substituteParameters in Jacobi polynomial -> Legendre" $ do
      let 
        x = qlone 1
        jacobi   = jacobiPolynomial 5
        legendre = (63*^x^**^5 ^-^ 70*^x^**^3 ^+^ 15*^x) /^ 8 
      assertEqual "" legendre (substituteParameters jacobi [0, 0])

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

    , testCase "changeVariables in RatioOfSprays" $ do
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

    , testCase "changeVariables in constant RatioOfSprays" $ do
      let
        rOS = constantRatioOfSprays 3 :: RatioOfQSprays
        x = qlone 1
        y = qlone 2
        u = x ^*^ y
        v = x^**^2 ^-^ y ^+^ unitSpray 
      assertEqual "" rOS (changeVariables rOS [u, v])

    , testCase "changeVariables in Polynomial" $ do
      let
        f :: Polynomial Rational -> Polynomial Rational 
        f p = (constPoly (2::Rational) AlgRing.* p) AlgRing.^ 3  
                AlgAdd.-  p  AlgAdd.+  constPoly 4
        a = soleParameter
        pol = f a
        u = a AlgRing.^ 2  AlgAdd.-  constPoly 10
      assertEqual "" (f u) (changeVariables pol [u])

    , testCase "fromRatioOfQPolynomials" $ do
      let
        a = qsoleParameter
        x = qlone 1
        rOP = ((a AlgRing.^ 8 AlgAdd.- AlgRing.one) NR.% 
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
        t2 = asRatioOfSprays 
              (((alpha0 ^+^ cst 2)^*^(alpha0 ^+^ beta0 ^+^ cst 3)) /^ 2) 
        t3 = asRatioOfSprays 
          (((alpha0 ^+^ beta0 ^+^ cst 3)^*^(alpha0 ^+^ beta0 ^+^ cst 4)) /^ 8)
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
        rOS' = 
          (3%4 :: Rational) AlgMod.*> rOS AlgRing.^ 2 AlgAdd.+ p AlgMod.*> rOS
        test2 = p AlgMod.*> (rOS' %/% p) == rOS'
        test3 = rOS1 %/% p == p %//% q
        test4 = rOS' AlgField./ rOS' == unitRatioOfSprays
        k = 3 :: Rational
        test5 = (p /> k) AlgMod.*> rOS1 == p AlgMod.*> (rOS1 /> k)
      assertEqual "" 
        [test1, test2, test3, test4, test5] [True, True, True, True, True]

    , testCase "arithmetic on univariate ratio of sprays" $ do
      let
        x = qlone 1  
        y = qlone 2 
        f :: QSpray -> RatioOfQSprays
        f p = 
          ((p^**^4 ^-^ 3*^p^**^3 ^+^ p^**^2)^**^2 %//% (p^**^2 ^+^ unitSpray)) 
          AlgAdd.+
          ((p^**^2 ^-^ unitSpray) %//% (p^**^2 ^+^ 3*^p)^**^2) AlgRing.* 
            ((p^**^3 ^-^ p^**^2 ^+^ unitSpray) %//% (2*^p^**^3 ^-^ unitSpray))
        rOSx = f x
        rOSy = f y
      assertEqual "" rOSx (swapVariables (1, 2) rOSy)

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

    , testCase "evalRatioOfSprays'" $ do
      let
        x = qlone 1
        y = qlone 2
        z = qlone 3
        p = x ^+^ y
        q = x ^-^ y
        rOS = p %//% q
        rOS1 = x %//% y
        rOS2 = z %//% x
        obtained = evalRatioOfSprays' rOS [rOS1, rOS2]
        expected = 
          (rOS1 AlgAdd.+ rOS2) AlgField./ (rOS1 AlgAdd.- rOS2)
      assertEqual "" obtained expected

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
      assertEqual "" 
        (7 .^ p, (-7) .^ p, 0 .^ p) (spray, AlgAdd.negate spray, zeroSpray)

    , testCase "collinearSprays" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        term1 = (2 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2))
        term2 = (4 % 1) *^ z
        term3 = (5 % 1) *^ unitSpray
        spray1 = term1 ^+^ term2 ^+^ term3
        spray2 = 121 *^ (term3 ^+^ term1 ^+^ term2)
      assertBool "" (collinearSprays spray1 spray2)
    
    , testCase "bombieriSpray" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        poly =
          (2 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ 
            (4 % 1) *^ z ^+^ (5 % 1) *^ unitSpray
        bpoly =
          (24 % 1) *^ ((2 % 1) *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ 
            (4 % 1) *^ z ^+^ (5 % 1) *^ unitSpray
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
      assertEqual "" 
        (getCoefficient [3, 2, 0] p, getCoefficient [0, 4] p) (4, 0),

    testCase "getConstantTerm" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p = 2 *^ (2 *^ (x^**^3 ^*^ y^**^2)) ^+^ 4 *^ z ^+^ 5 *^ unitSpray
      assertEqual "" (getConstantTerm p) 5

    , testCase "fromList . toList = identity" $ do
      let
        x = lone 1 :: Spray Int
        y = lone 2 :: Spray Int
        z = lone 3 :: Spray Int
        p = 2 *^ (2 *^ (x ^**^ 3 ^*^ y ^**^ 2)) ^+^ 4 *^ z ^+^ 5 *^ unitSpray
      assertEqual "" p (fromList . toList $ p)

    , testCase "toList . fromList cleans the list" $ do
      let
        l = [([], 2), ([3, 1], 4), ([3, 1], 6), ([2, 0, 0], 5), ([9, 3], 0)]
        spray = fromList l :: Spray Int
        l' = sortOn fst (toList spray)
      assertEqual "" l' [([], 2), ([2], 5), ([3, 1], 10)]

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
        g = groebnerBasis [p1, p2, p3] True
        xyz = [sqrt 2 - 1, sqrt 2 - 1, sqrt 2 - 1]
        gxyz = map ((`evalSpray` xyz) . fromRationalSpray) g
        sumAbsValues = sum $ map abs gxyz
      assertApproxEqual "" 8 sumAbsValues 0

    , testCase "symmetric polynomial" $ do
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
        p = x^**^3 ^*^ y^**^2 ^*^ z ^+^ x^**^3 ^*^ y ^*^ z^**^2 ^+^ 
          x^**^2 ^*^ y^**^3 ^*^ z ^+^ 2*^(x^**^2 ^*^ y^**^2 ^*^ z^**^2) ^+^ 
          x^**^2 ^*^ y ^*^ z^**^3 ^+^ x ^*^ y^**^3 ^*^ z^**^2 ^+^ 
          x ^*^ y^**^2 ^*^ z^**^3
      assertBool "" (isSymmetricSpray p),
 
    testCase "isPolynomialOf" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p1 = x ^+^ y
        p2 = x ^-^ y
        p = p1 ^*^ p2 ^+^ unitSpray
      assertEqual "" 
        (isPolynomialOf p [p1, p2]) (True, Just $ x ^*^ y ^+^ unitSpray),

    testCase "isPolynomialOf - 2" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
      assertEqual "" 
        (isPolynomialOf x [x ^+^ y^*^z, y, z]) 
        (True, Just $ x ^-^ y^*^z)

    , testCase "Groebner implicitization ellipse" $ do
      let
        cost = qlone 1
        sint = qlone 2
        n_variables = 2
        a = qlone 3
        b = qlone 4
        equations = [a ^*^ cost, b ^*^ sint]
        relations = [cost^**^2 ^+^ sint^**^2 ^-^ unitSpray]
        m = maximum (map numberOfVariables equations)
        coordinates = [qlone (m + i) | i <- [1 .. length equations]]
        generators = relations ++ zipWith (^-^) equations coordinates 
        gb = groebnerBasis generators True
        isfree :: QSpray -> Bool
        isfree spray = not $ any (involvesVariable spray) [1 .. n_variables]
        results = filter isfree gb
        results' = map (dropVariables n_variables) results 
        showResults = map (prettyQSprayXYZ ["a", "b", "x", "y"]) results'
      assertEqual "" showResults ["a^2.b^2 - a^2.y^2 - b^2.x^2"]

    , testCase "Groebner implicitization Enneper" $ do
      let
        u = qlone 1
        v = qlone 2
        x = 3*^u ^+^ 3*^(u ^*^ v^**^2) ^-^ u^**^3
        y = 3*^v ^+^ 3*^(u^**^2 ^*^ v) ^-^ v^**^3
        z = 3*^u^**^2 ^-^ 3*^v^**^2
        generators = [x ^-^ qlone 3, y ^-^ qlone 4, z ^-^ qlone 5]
        gb = groebnerBasis generators True
        isfree :: QSpray -> Bool
        isfree spray = 
          not (involvesVariable spray 1) && not (involvesVariable spray 2)
        results = filter isfree gb
        results' = map (dropVariables 2) results 
        xyz = map (evaluateAt [1%4, 2%3]) [x, y, z]
        equation = results' !! 0
        shouldBeZero = evaluateAt xyz equation
      assertEqual "" (length results', shouldBeZero) (1, 0)

    , testCase "power sum polynomials" $ do
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
        f :: Spray Rational -> Spray Rational -> Spray Rational 
          -> Spray Rational
        f p1 p2 p3 = 
          p1^**^4 ^+^ (2 *^ p2^**^3) ^+^ (3 *^ p3^**^2) ^-^ (4 *^ unitSpray)
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        p = f x1 x2 x3
        p' = permuteVariables [3, 1, 2] p
      assertEqual "" p' (f x3 x1 x2)

    , testCase "permuteVariables can add a new variable" $ do
      let
        f :: Spray Rational -> Spray Rational -> Spray Rational 
          -> Spray Rational
        f p1 p2 p3 = 
          p1^**^4 ^+^ (2 *^ p2^**^3) ^+^ (3 *^ p3^**^2) ^-^ (4 *^ unitSpray)
        x1 = lone 1 :: Spray Rational
        x2 = lone 2 :: Spray Rational
        x3 = lone 3 :: Spray Rational
        x4 = lone 4 :: Spray Rational
        p = f x1 x2 x3
        p' = permuteVariables [2, 1, 4, 3] p
      assertEqual "" p' (f x2 x1 x4)

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
        f p1 p2 p3 = (p1^**^4 ^+^ (2 *^ p2^**^3)) %//% 
                      ((3 *^ p3^**^2) ^-^ (4 *^ unitSpray))
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
        rOS = (x1^**^4 ^+^ (2 *^ x2^**^3)) %//% 
                ((3 *^ x3^**^2) ^-^ (4 *^ unitSpray))
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
      assertEqual "" 
        r (16*^x^**^8 ^-^ 32*^x^**^7 ^+^ 24*^x^**^6 ^-^ 8*^x^**^5 ^+^ x^**^4),

    testCase "resultant product rule" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        f = x^**^4 ^-^ x^**^3 ^+^ x^**^2 ^-^ 2*^ (x ^*^ y^**^2) ^+^ y^**^4 
        g = x ^-^ (2*^ y^**^2)
        h = x^**^2 ^*^ y ^+^ y^**^3 ^+^ unitSpray
      assertEqual "" 
        (resultant 1 (f^*^g) h) (resultant 1 f h ^*^ resultant 1 g h),

    testCase "subresultants" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        p = x^**^2 ^*^ y ^*^ (y^**^2 ^-^ 5*^ x ^+^ constantSpray 6) 
        q = x^**^2 ^*^ y ^*^ (3*^ y ^+^ constantSpray 2)
        sx = subresultants 1 p q
      assertBool "" 
        (sx!!0 == zeroSpray && sx!!1 == zeroSpray && sx!!2 /= zeroSpray),

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
      assertBool "" (test1 && test2 && test3)

    , testCase "polynomialSubresultants" $ do
      let
        x = qlone 1
        y = qlone 2
        p = x^**^3^*^y^**^3 ^+^ x^**^2 ^+^ 3*^x ^+^ unitSpray
        q = x^**^4 ^+^ 2*^x^**^3 ^-^ x^*^y^**^4
        srs1 = map prettyQSpray (polynomialSubresultants 1 p q)
        srs2 = map prettyQSpray (polynomialSubresultants 2 p q)
      assertEqual "" 
        (srs1, srs2)
        (
          [
            "y^21 + 15*y^14 - 11*y^11 + y^8 + 37*y^7 + 4*y^4 - 8*y^3 - 1"
          , "x.y^17 + 14*x.y^10 - 11*x.y^7 + x.y^4 + 39*x.y^3 + 5*x + 2*y^10 - y^7 + 14*y^3 + 2"
          , "-5*x^2.y^3 + x^2 - x.y^10 - 7*x.y^3 + 3*x - 2*y^3 + 1"
          , "x^3.y^3 + x^2 + 3*x + 1"
          ],
          [
            "x^24 + 6*x^23 + 12*x^22 + 8*x^21 - x^11 - 12*x^10 - 58*x^9 - 144*x^8 - 195*x^7 - 144*x^6 - 58*x^5 - 12*x^4 - x^3"
          , "x^13 + 5*x^12 + 7*x^11 + 2*x^10 + x^9.y + 6*x^8.y + 11*x^7.y + 6*x^6.y + x^5.y"
          , "x^10 + 2*x^9 + x^6.y + 3*x^5.y + x^4.y"
          , "x^3.y^3 + x^2 + 3*x + 1"
          ]
        )

    , testCase "principalSturmHabichtSequence" $ do
      let
        x = qlone 1
        y = qlone 2
        p = x^**^4 ^*^ y ^+^ y^**^2
        psh1 = principalSturmHabichtSequence 1 p
        psh2 = principalSturmHabichtSequence 2 p
      assertEqual "" 
        (psh1, psh2)
        (
          [
              256 *^ y^**^10
            , zeroSpray
            , zeroSpray
            , 4 *^ y
            , y
          ],
          [
              x^**^8
            , constantSpray 2
            , constantSpray 1
          ]
        )

    , testCase "sturmHabichtSequence & principalSturmHabichtSequence" $ do
      let
        x = qlone 1
        y = qlone 2
        p = x^**^3^*^y^**^4 ^+^ x^**^2^*^y^**^2 ^+^ 3*^y ^-^ constantSpray 6
        sh1 = map prettyQSpray (sturmHabichtSequence 1 p)
        sh2 = map prettyQSpray (sturmHabichtSequence 2 p)
        psh1 = map prettyQSpray (principalSturmHabichtSequence 1 p)
        psh2 = map prettyQSpray (principalSturmHabichtSequence 2 p)
      assertEqual "" 
        ((sh1, psh1), (sh2, psh2))
        (
          (
            [
              "-243*y^14 + 972*y^13 - 972*y^12 - 12*y^11 + 24*y^10"
            , "2*x.y^8 - 27*y^9 + 54*y^8"
            , "3*x^2.y^4 + 2*x.y^2"
            , "x^3.y^4 + x^2.y^2 + 3*y - 6"
            ],
            [
              "-243*y^14 + 972*y^13 - 972*y^12 - 12*y^11 + 24*y^10"
            , "2*y^8"
            , "3*y^4"
            , "y^4"
            ]
          ),
          (
            [
              "-96*x^14 - 4608*x^13 - 55332*x^12 - 7776*x^11 - 2187*x^9"
            , "-8*x^12.y - 192*x^11.y - 12*x^10 - 324*x^9.y + 864*x^9"
            , "-8*x^8.y^2 - 36*x^6.y + 96*x^6"
            , "4*x^3.y^3 + 2*x^2.y + 3"
            , "x^3.y^4 + x^2.y^2 + 3*y - 6"
            ],
            [
              "-96*x^14 - 4608*x^13 - 55332*x^12 - 7776*x^11 - 2187*x^9"
            , "-8*x^12 - 192*x^11 - 324*x^9"
            , "-8*x^8"
            , "4*x^3"
            , "x^3"
            ]
          )
        )

    , testCase "number of real roots in interval" $ do
      let
        x = qlone 1
        factors = [x ^-^ constantSpray (toRational i) | i <- [1::Int .. 5]]
        spray = productOfSprays factors
        intervals = [
            (Just 0, Just 9)
          , (Just 1, Just 6)
          , (Just 2, Just 3)
          , (Just 0, Just 4)
          , (Just $ 2 + (1%4), Just $ 3 - (1%4))
          ]
        nroots = 
          map (uncurry (numberOfRealRootsInClosedInterval spray)) intervals
        nroots' = 
          map (uncurry (numberOfRealRootsInOpenInterval spray)) intervals
        rightUnboundedIntervals = map ((, Nothing) . Just) [0, 1, 2, 2 + (1%4), 5]
        nroots'' = map (uncurry (numberOfRealRootsInClosedInterval spray)) 
                        rightUnboundedIntervals
        nroots''' = map (uncurry (numberOfRealRootsInOpenInterval spray)) 
                          rightUnboundedIntervals
        leftUnboundedIntervals = map ((Nothing, ) . Just) [0, 1, 2, 2 + (1%4), 5]
        nroots'''' = map (uncurry (numberOfRealRootsInClosedInterval spray)) 
                        leftUnboundedIntervals
        nroots''''' = map (uncurry (numberOfRealRootsInOpenInterval spray)) 
                          leftUnboundedIntervals
        ntotalroots = numberOfRealRootsInClosedInterval spray Nothing Nothing
      assertEqual "" 
        ( nroots, nroots'
        , nroots'', nroots'''
        , nroots'''', nroots'''''
        , ntotalroots
        ) 
        ( [5, 5, 2, 4, 0], [5, 4, 0, 3, 0]
        , [5, 5, 4, 3, 1], [5, 4, 3, 3, 0]
        , [0, 1, 2, 2, 5], [0, 0, 1, 2, 4]
        , 5
        )

    , testCase "number of real roots" $ do
      let
        x = qlone 1
        factors = [x ^-^ constantSpray (toRational i) | i <- [1::Int .. 5]]
        spray = productOfSprays factors
        nroots = map numberOfRealRoots [spray, x^**^3, x^**^2 <+ 1]
      assertEqual "" nroots [5, 1, 0]

    , testCase "gcdSpray - univariate example" $ do
      let
        x = lone 1 :: Spray Rational
        sprayD = x^**^2 ^+^ unitSpray
        sprayA = sprayD ^*^ (x^**^4 ^-^ x) 
        sprayB = sprayD ^*^ (2*^x ^+^ unitSpray)
        sprayGCD = gcdSpray sprayA sprayB
      assertEqual "" sprayGCD sprayD,

    testCase "gcdSpray with a constant spray" $ do
      let
        x = lone 1 :: Spray Rational
        sprayA = 3 *^ x^**^4 ^-^ x 
        b1 = 2 :: Rational
        b2 = 4 :: Rational
        sprayB1 = constantSpray b1
        sprayB2 = constantSpray b2
      assertBool "" 
        (gcdSpray sprayA sprayB1 == unitSpray && 
          gcdSpray sprayA sprayB2 == unitSpray)

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
      assertEqual "" g sprayD

    , testCase "gcdSpray - trivariate example" $ do
      let 
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        sprayD = x^**^2 ^*^ y  ^-^  x ^*^ y  ^+^  z  ^+^  constantSpray 3
        sprayA = 
          sprayD^**^1 ^*^ (x^**^4  ^-^  x  ^+^  y   ^+^  x ^*^ y ^*^ z^**^2)
        sprayB = sprayD^**^1 ^*^ y ^*^ (2*^x  ^+^  unitSpray) ^*^ z
        g = gcdSpray sprayA sprayB
      assertEqual "" g sprayD

    , testCase "gcdSpray is commutative" $ do
      let 
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        sprayA = (-5)*^x^**^3 ^-^ 11*^(x^**^2 ^*^ y) ^-^ 11*^(x^*^ y^**^2) ^-^ 
            8*^(x ^*^ y ^*^ z) ^+^ 20*^(x ^*^ z^**^2) ^-^ 5 *^ y^**^3 ^+^ 
              20*^(y ^*^ z^**^2)
        sprayB = x^*^y ^-^ x^*^z ^-^ y^*^z ^+^ z^**^2
      assertEqual "" (gcdSpray sprayA sprayB) (gcdSpray sprayB sprayA)

    , testCase "evaluation of one-parameter spray" $ do
      let 
        a    = qsoleParameter
        p    = a AlgRing.^ 2 AlgAdd.- constQPoly 4 
        q1   = a AlgAdd.- constQPoly 3
        q2   = a AlgAdd.- constQPoly 2
        rop1 = p :% q1 
        rop2 = p :% q2
        f :: (Eq a, AlgRing.C a) 
          => Spray a -> Spray a -> Spray a -> (Spray a, Spray a)
        f x y z = (x^**^2 ^+^ y^**^2, z)
        g :: (Eq a, AlgRing.C a) 
          => Spray a -> Spray a -> Spray a -> (a, a, a) -> (a, a)
        g px py pz (x, y, z) = 
          (
            evalSpray f1 [x, y, AlgAdd.zero]
          , evalSpray f2 [AlgAdd.zero, AlgAdd.zero, z]
          )
          where (f1, f2) = f px py pz
        (r1, r2) = g (lone 1 :: QSpray') (lone 2) (lone 3) (2, 3, 4) 
        r = evalRatioOfPolynomials 5 rop1 AlgRing.* r1  AlgAdd.+  
              evalRatioOfPolynomials 5 rop2 AlgRing.* r2
        (f1', f2')  = f (lone 1 :: OneParameterQSpray) (lone 2) (lone 3)
        opSpray  = rop1 *^ f1'  ^+^  rop2 *^ f2' 
        r' = evalOneParameterSpray' opSpray 5 [2, 3, 4]
        rop1' = evalOneParameterSpray'' f1' [2, 3]
        rop2' = evalOneParameterSpray'' f2' [0, 0, 4]
        r'' = evalRatioOfPolynomials 5 
                (rop1 AlgRing.* rop1' AlgAdd.+ rop2 AlgRing.* rop2')
        spray = substituteParameters opSpray [5]
        r''' = evalSpray spray [2, 3, 4]
      assertEqual "" ((r, r'), r'') ((r', r''), r'''),

    testCase "pretty spray" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        p1 = ((2%3) *^ x^**^3) ^*^ y  ^-^  x^**^2  ^+^  
                y ^*^ z  ^-^  (2%3) *^ unitSpray
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
          = ((4 NR.% 5) *^ (a :% (a AlgRing.^ 2 AlgAdd.+ AlgRing.one))) 
              *^ (x^**^2 ^-^ y^**^2)  
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
          = ((4 NR.% 5) *^ (a :% (a AlgRing.^ 2 AlgAdd.+ AlgRing.one))) 
              *^ (x^**^2 ^-^ y^**^2)  
              ^+^  (constQPoly (2 NR.% 3) AlgRing.* a) AlgMod.*> (y ^*^ z)
        x' = lone 1 :: ParametricSpray Rational'
        y' = lone 2 :: ParametricSpray Rational'
        z' = lone 3 :: ParametricSpray Rational'
        a' = lone 1 :: Spray Rational'
        spray  
          = ((4 NR.% 5 :: Rational') AlgMod.*> 
                (a' %:% (a'^**^2 ^+^ unitSpray))) *^ (x'^**^2 ^-^ y'^**^2)  
            ^+^  ((2 NR.% 3) *^ a') AlgMod.*> (y' ^*^ z')
      assertEqual "" (parametricSprayToOneParameterSpray spray) sSpray


  ]
