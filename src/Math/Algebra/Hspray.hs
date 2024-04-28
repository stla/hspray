{-|
Module      : Math.Algebra.Hspray
Description : Multivariate polynomials on a ring.
Copyright   : (c) Stéphane Laurent, 2022-2024
License     : GPL-3
Maintainer  : laurent_step@outlook.fr

Deals with multivariate polynomials on a commutative ring. 
See README for examples.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Algebra.Hspray
  ( 
  -- * Classes
    HasVariables (..)
  , isConstant
  , isUnivariate
  , isBivariate
  , isTrivariate
  -- * Main types
  , Powers (..)
  , Spray
  , QSpray
  , QSpray'
  , Term
  -- * Basic sprays
  , lone
  , qlone
  , lone'
  , qlone'
  , monomial
  , qmonomial
  , unitSpray
  , zeroSpray
  , constantSpray
  -- * Operations on sprays
  , (*^)
  , (/^)
  , (^+^)
  , (^-^)
  , (^*^)
  , (^**^)
  -- * Showing a spray
  , prettySpray
  , prettySpray'
  , prettySpray''
  , prettySprayXYZ
  , prettySprayX1X2X3
  , showSpray
  , showSprayXYZ
  , showSprayXYZ'
  , showSprayX1X2X3
  , showSprayX1X2X3'
  , showNumSpray
  , showQSpray
  , showQSpray'
  , prettyNumSprayX1X2X3
  , prettyQSprayX1X2X3
  , prettyQSprayX1X2X3'
  , prettyNumSprayXYZ
  , prettyQSprayXYZ
  , prettyQSprayXYZ'
  , prettyNumSpray
  , prettyNumSpray'
  , prettyQSpray
  , prettyQSpray''
  , prettyQSpray'
  , prettyQSpray'''
  -- * Univariate polynomials and fractions of univariate polynomials
  , A (..)
  , Rational'
  , Q
  , scalarQ
  , Polynomial 
  , RatioOfPolynomials
  , QPolynomial 
  , RatioOfQPolynomials
  , prettyRatioOfPolynomials
  , prettyRatioOfQPolynomials
  , (*.)
  , constPoly
  , polyFromCoeffs
  , soleParameter
  , constQPoly
  , qpolyFromCoeffs
  , qsoleParameter
  , evalRatioOfPolynomials
  -- * One-parameter sprays 
  , OneParameterSpray
  , OneParameterQSpray
  , prettyOneParameterSprayX1X2X3
  , prettyOneParameterSprayXYZ
  , prettyOneParameterSpray
  , prettyOneParameterSpray'
  , prettyOneParameterQSprayX1X2X3
  , prettyOneParameterQSprayXYZ
  , prettyOneParameterQSpray
  , prettyOneParameterQSpray'
  , evalOneParameterSpray
  , substituteTheParameter
  , evalOneParameterSpray'
  , evalOneParameterSpray''
  -- * Ratios of sprays
  , RatioOfSprays (..)
  , RatioOfQSprays
  , (%:%)
  , (%//%)
  , (^/^)
  , (%/%)
  , isConstantRatioOfSprays
  , isPolynomialRatioOfSprays
  , zeroRatioOfSprays
  , zeroROS
  , unitRatioOfSprays
  , unitROS
  , constantRatioOfSprays
  , asRatioOfSprays
  , evalRatioOfSprays
  , substituteRatioOfSprays
  , fromRatioOfPolynomials
  , fromRatioOfQPolynomials
  , showRatioOfSprays
  , showRatioOfNumSprays
  , showRatioOfQSprays
  , showRatioOfSpraysXYZ
  , showRatioOfSpraysXYZ'
  , showRatioOfSpraysX1X2X3
  , showRatioOfSpraysX1X2X3'
  , prettyRatioOfQSpraysXYZ
  , prettyRatioOfQSpraysX1X2X3
  , prettyRatioOfQSprays
  , prettyRatioOfQSprays'
  , prettyRatioOfNumSpraysXYZ
  , prettyRatioOfNumSpraysX1X2X3
  , prettyRatioOfNumSprays
  , prettyRatioOfNumSprays'
  -- * Parametric sprays
  --
  -- | There are three types of parametric sprays: @OneParameterSpray@, 
  -- @SimpleParametricSpray@ and @ParametricSpray@. These are sprays of 
  -- type @Spray b@ where @b@ has the class @HasVariables@. When we say 
  -- \"parametric spray\" in the documentation, we mean either 
  -- such a spray or more precisely a @ParametricSpray@ spray.
  , SimpleParametricSpray
  , SimpleParametricQSpray
  , ParametricSpray
  , ParametricQSpray
  , canCoerceToSimpleParametricSpray
  , asSimpleParametricSprayUnsafe
  , asSimpleParametricSpray
  , fromOneParameterSpray
  , fromOneParameterQSpray
  , parametricSprayToOneParameterSpray
  , parametricQSprayToOneParameterQSpray
  , gegenbauerPolynomial
  , jacobiPolynomial
  , numberOfParameters
  , changeParameters
  , substituteParameters
  , evalParametricSpray
  , evalParametricSpray'
  , prettyParametricQSprayABCXYZ
  , prettyParametricQSpray
  , prettyParametricNumSprayABCXYZ
  , prettyParametricNumSpray
  , prettySimpleParametricQSprayABCXYZ
  , prettySimpleParametricQSpray
  , prettySimpleParametricNumSprayABCXYZ
  , prettySimpleParametricNumSpray
  -- * Queries on a spray
  , getCoefficient
  , getConstantTerm
  , isConstantSpray
  , sprayTerms
  -- * Evaluation of a spray
  , evalSpray
  , substituteSpray
  , composeSpray
  , evalSpraySpray
  -- * Division of a spray
  , sprayDivision
  , sprayDivisionRemainder
  -- * Gröbner basis
  , groebner
  , reduceGroebnerBasis
  -- * Symmetric polynomials
  , esPolynomial
  , psPolynomial
  , isSymmetricSpray
  -- * Resultant and subresultants
  , resultant
  , resultant'
  , resultant1
  , subresultants
  , subresultants1
  -- * Greatest common divisor
  , gcdSpray
  -- * Matrices
  , detLaplace
  , detLaplace'
  , characteristicPolynomial
  -- * Miscellaneous
  , (.^)
  , (/>)
  , fromList
  , toList
  , fromRationalSpray
  , leadingTerm
  , isPolynomialOf
  , bombieriSpray
  , collinearSprays
  , quotientsByGCD
  ) where
import qualified Algebra.Additive              as AlgAdd
import qualified Algebra.Differential          as AlgDiff
import qualified Algebra.Field                 as AlgField
import qualified Algebra.Module                as AlgMod
import qualified Algebra.RightModule           as AlgRightMod
import qualified Algebra.Ring                  as AlgRing
import qualified Algebra.ZeroTestable          as AlgZT
import qualified Data.Foldable                 as DF
import           Data.Function                  ( on )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.Hashable                  ( Hashable ( hashWithSalt ) )
import qualified Data.IntMap.Strict            as IM
import           Data.List                      ( sortBy
                                                , maximumBy 
                                                , (\\)
                                                , findIndices
                                                , elemIndices
                                                , nub
                                                , foldl1'
                                                , uncons
                                                )
import           Data.Matrix                    ( Matrix 
                                                , fromLists
                                                , minorMatrix
                                                , nrows
                                                , ncols
                                                , submatrix
                                                )
import qualified Data.Matrix                   as DM
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                , fromJust
                                                , fromMaybe
                                                )
import           Data.Ord                       ( comparing )
import qualified Data.Ratio                    as DR
import qualified GHC.Real                      as DR
import qualified Data.Sequence                 as S
import           Data.Sequence                  ( (><)
                                                , Seq 
                                                , dropWhileR
                                                , (|>)
                                                , index
                                                , adjust
                                                , fromFunction
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , cons
                                                , intercalate
                                                , pack
                                                , snoc
                                                , unpack
                                                )
import           Data.Tuple.Extra               ( both, first )
import qualified MathObj.Matrix                as MathMatrix
import qualified MathObj.Polynomial            as MathPol
import           Number.Ratio                   ( T ( (:%) ), (%) )
import qualified Number.Ratio                  as NumberRatio
-- import qualified Algebra.PrincipalIdealDomain  as AlgPID
-- import qualified Algebra.Units  as AlgUnits
-- import qualified Algebra.IntegralDomain  as AlgID


-- Classes --------------------------------------------------------------------

-- | A spray represents a multivariate polynomial so it has some variables. We 
-- introduce a class because it will be assigned to the ratios of sprays too.
class HasVariables b where

  -- | Number of variables
  numberOfVariables :: b -> Int

  -- | Permutes the variables
  --
  -- >>> f :: Spray Rational -> Spray Rational -> Spray Rational -> Spray Rational
  -- >>> f p1 p2 p3 = p1^**^4 ^+^ (2*^p2^**^3) ^+^ (3*^p3^**^2) ^-^ (4*^unitSpray)
  -- >>> x1 = lone 1 :: Spray Rational
  -- >>> x2 = lone 2 :: Spray Rational
  -- >>> x3 = lone 3 :: Spray Rational
  -- >>> spray = f x1 x2 x3
  --
  -- prop> permuteVariables [3, 1, 2] spray == f x3 x1 x2
  permuteVariables :: 
       [Int] -- ^ permutation 
    -> b     -- ^ the object whose variables will be permuted
    -> b     -- ^ the object with permuted variables

  -- | Swaps two variables 
  -- 
  -- prop> swapVariables (1, 3) x == permuteVariables [3, 2, 1] x
  swapVariables :: 
       (Int, Int) -- ^ the indices of the variables to be swapped (starting at 1) 
    -> b          -- ^ the object whose variables will be swapped
    -> b          -- ^ the object with swapped variables

  -- | Derivative 
  --
  -- >>> x = lone 1 :: Spray Int
  -- >>> y = lone 2 :: Spray Int
  -- >>> spray = 2*^x ^-^ 3*^y^**^8
  -- >>> spray' = derivative 1 spray
  -- >>> putStrLn $ prettyNumSpray spray'
  -- 2
  derivative :: 
       Int -- ^ index of the variable of differentiation (starting at 1)
    -> b   -- ^ the object to be derivated
    -> b   -- ^ the derivated object

  -- | The type of the coefficients (this is @a@ for both @Spray a@ and @RatioOfSprays a@)
  type family BaseRing b

  -- | The type of the variables (this is @Spray a@ for both @Spray a@ and @RatioOfSprays a@)
  type family VariablesType b

  -- | Evaluation (replacing the variables with some values)
  --
  -- >>> x = lone 1 :: Spray Int
  -- >>> y = lone 2 :: Spray Int
  -- >>> spray = 2*^x^**^2 ^-^ 3*^y
  -- >>> evaluate spray [2, 1]
  -- 5
  evaluate :: b -> [BaseRing b] -> BaseRing b

  -- | Flipped version of @evaluate@
  --
  -- >>> x = lone 1 :: Spray Int
  -- >>> y = lone 2 :: Spray Int
  -- >>> spray = 2*^x^**^2 ^-^ 3*^y
  -- >>> evaluateAt [2, 1] spray
  -- 5
  evaluateAt :: [BaseRing b] -> b -> BaseRing b
  evaluateAt = flip evaluate

  -- | Substitution (partial evaluation)
  --
  -- >>> x1 = lone 1 :: Spray Int
  -- >>> x2 = lone 2 :: Spray Int
  -- >>> x3 = lone 3 :: Spray Int
  -- >>> spray = x1^**^2 ^-^ x2 ^+^ x3 ^-^ unitSpray
  -- >>> spray' = substitute [Just 2, Nothing, Just 3] spray
  -- >>> putStrLn $ prettyNumSprayX1X2X3 "x" spray'
  -- -x2 + 6 
  substitute :: [Maybe (BaseRing b)] -> b -> b

  -- | Change variables
  --
  -- >>> x = lone 1 :: Spray Int
  -- >>> y = lone 2 :: Spray Int
  -- >>> spray = x ^*^ y
  -- >>> spray' = changeVariables spray [x ^+^ y, x ^-^ y]
  -- >>> putStrLn $ prettyNumSpray' spray'
  -- X^2 - Y^2
  changeVariables :: 
       b                 -- ^ object with variables such as a spray
    -> [VariablesType b] -- ^ list of new variables
    -> b

-- | Whether an object of class `HasVariables` is constant
isConstant :: HasVariables b => b -> Bool
isConstant f = numberOfVariables f == 0

-- | Whether an object of class `HasVariables` is univariate; it is considered 
-- that it is univariate if it is constant
isUnivariate :: HasVariables b => b -> Bool
isUnivariate f = numberOfVariables f <= 1

-- | Whether an object of class `HasVariables` is bivariate; it is considered 
-- that it is bivariate if it is univariate
isBivariate :: HasVariables b => b -> Bool
isBivariate f = numberOfVariables f <= 2

-- | Whether an object of class `HasVariables` is trivariate; it is considered 
-- that it is trivariate if it is bivariate
isTrivariate :: HasVariables b => b -> Bool
isTrivariate f = numberOfVariables f <= 3


-- Additional operations to 'numeric-prelude'

infixr 7 />
-- | Divides by a scalar in a module over a field
(/>) :: (AlgField.C k, AlgMod.C k a) => a -> k -> a
x /> lambda = AlgField.recip lambda AlgMod.*> x

infixr 7 .^
-- | Scale by an integer (I do not find this operation in __numeric-prelude__)
--
-- prop> 3 .^ x == x Algebra.Additive.+ x Algebra.Additive.+ x
(.^) :: (AlgAdd.C a, Eq a) => Int -> a -> a
k .^ x = if k >= 0
  then powerOperation (AlgAdd.+) AlgAdd.zero x k
  else (.^) (-k) (AlgAdd.negate x)
  where 
    powerOperation op =
      let go acc _ 0 = acc
          go acc a n = go (if even n then acc else op acc a) (op a a) (div n 2)
      in go


-- Univariate polynomials and ratios of univariate polynomials ----------------

newtype A a = A a 
  deriving
    (Eq, Show, AlgAdd.C, AlgRing.C, AlgField.C)

type Rational' = NumberRatio.Rational
type Q = A Rational'

-- | Identify a rational to a @A Rational'@ element
scalarQ :: Rational' -> Q
scalarQ = A 

type Polynomial a         = MathPol.T (A a)
type RatioOfPolynomials a = NumberRatio.T (Polynomial a)
type QPolynomial          = Polynomial Rational'
type RatioOfQPolynomials  = RatioOfPolynomials Rational'

instance (Eq a, AlgField.C a) => HasVariables (Polynomial a) where
  --
  numberOfVariables :: Polynomial a -> Int
  numberOfVariables p = case MathPol.degree p of
    Nothing -> 0
    Just d  -> min 1 d
  --
  type BaseRing (Polynomial a) = a
  --
  type VariablesType (Polynomial a) = Polynomial a
  --
  evaluate :: Polynomial a -> [a] -> a
  evaluate p xs = get (MathPol.evaluate p (A (xs !! 0)))
    where
      get (A x) = x
  --
  substitute :: [Maybe a] -> Polynomial a -> Polynomial a
  substitute x p = 
    if isNothing (x !! 0)
      then p
      else constPoly (evaluate p [fromJust $ x !! 0])
  -- 
  permuteVariables :: [Int] -> Polynomial a -> Polynomial a
  permuteVariables = error "permuteVariables: there is only one variable."
  -- 
  swapVariables :: (Int, Int) -> Polynomial a -> Polynomial a
  swapVariables = error "swapVariables: there is only one variable."
  --
  derivative :: Int -> Polynomial a -> Polynomial a
  derivative i p = 
    if i == 1 
      then AlgDiff.differentiate p
      else constPoly AlgAdd.zero
  --
  changeVariables :: Polynomial a -> [Polynomial a] -> Polynomial a
  changeVariables p ps = MathPol.compose p (ps !! 0)

instance (Eq a, AlgField.C a) => HasVariables (RatioOfPolynomials a) where
  numberOfVariables :: RatioOfPolynomials a -> Int
  numberOfVariables (p :% q) = 
    max (numberOfVariables p) (numberOfVariables q)
  --
  type BaseRing (RatioOfPolynomials a) = a
  --
  type VariablesType (RatioOfPolynomials a) = Polynomial a
  --
  evaluate :: RatioOfPolynomials a -> [a] -> a
  evaluate r xs = evaluate (NumberRatio.numerator r) xs AlgField./ 
    evaluate (NumberRatio.denominator r) xs
  --
  substitute :: [Maybe a] -> RatioOfPolynomials a -> RatioOfPolynomials a
  substitute x r = 
    if isNothing (x !! 0)
      then r
      else substitute x (NumberRatio.numerator r) %
        substitute x (NumberRatio.denominator r)
  -- 
  permuteVariables :: [Int] -> RatioOfPolynomials a -> RatioOfPolynomials a
  permuteVariables = error "permuteVariables: there is only one variable."
  -- 
  swapVariables :: (Int, Int) -> RatioOfPolynomials a -> RatioOfPolynomials a
  swapVariables = error "swapVariables: there is only one variable."
  --
  derivative :: Int -> RatioOfPolynomials a -> RatioOfPolynomials a
  derivative i r = 
    if i == 1 
      then 
        (p' AlgRing.* q AlgAdd.- p AlgRing.* q') % q AlgRing.^ 2
      else constPoly AlgAdd.zero :% constPoly AlgRing.one
        where 
          p = NumberRatio.numerator r
          q = NumberRatio.denominator r
          p' = AlgDiff.differentiate p
          q' = AlgDiff.differentiate q
  --
  changeVariables :: 
    RatioOfPolynomials a -> [Polynomial a] -> RatioOfPolynomials a
  changeVariables r ps = changeVariables (NumberRatio.numerator r) ps %
    changeVariables (NumberRatio.denominator r) ps 

{- -- | Division of univariate polynomials; this is an application of `:%` 
-- followed by a simplification of the obtained fraction of the two polynomials
(^/^) :: (Eq a, AlgField.C a) 
      => Polynomial a -> Polynomial a -> RatioOfPolynomials a
(^/^) pol1 pol2 = simplifyRatioOfPolynomials $ pol1 :% pol2 
 -}
instance (Eq a, AlgField.C a) => AlgZT.C (A a) where
  isZero :: A a -> Bool
  isZero (A r) = r == AlgAdd.zero

instance (Eq a, AlgField.C a) => AlgMod.C (A a) (RatioOfPolynomials a) where
  (*>) :: A a -> RatioOfPolynomials a -> RatioOfPolynomials a
  lambda *> rop = NumberRatio.scale (MathPol.const lambda) rop 

instance (Eq a, AlgField.C a) => AlgRightMod.C (A a) (RatioOfPolynomials a) 
  where
  (<*) :: RatioOfPolynomials a -> A a -> RatioOfPolynomials a
  rop <* lambda = lambda AlgMod.*> rop 

instance (Eq a, AlgField.C a) => AlgMod.C a (RatioOfPolynomials a) where
  (*>) :: a -> RatioOfPolynomials a -> RatioOfPolynomials a
  lambda *> rop = A lambda AlgMod.*> rop 

instance (Eq a, AlgField.C a) => AlgRightMod.C a (RatioOfPolynomials a) where
  (<*) :: RatioOfPolynomials a -> a -> RatioOfPolynomials a
  rop <* lambda = lambda AlgMod.*> rop 

instance (Eq a, AlgField.C a) 
          => AlgMod.C (Polynomial a) (RatioOfPolynomials a) 
  where
  (*>) :: Polynomial a -> RatioOfPolynomials a -> RatioOfPolynomials a
  p *> r = NumberRatio.scale p r 

instance (Eq a, AlgField.C a) 
          => AlgRightMod.C (Polynomial a) (RatioOfPolynomials a) 
  where
  (<*) :: RatioOfPolynomials a -> Polynomial a -> RatioOfPolynomials a
  r <* p = p AlgMod.*> r 

infixr 7 *.
-- | Scale a ratio of univariate polynomials by a scalar
(*.) :: (Eq a, AlgField.C a) => a -> RatioOfPolynomials a -> RatioOfPolynomials a
(*.) lambda rop = A lambda AlgMod.*> rop

-- | Constant univariate polynomial
constPoly :: a -> Polynomial a
constPoly x = MathPol.const (A x)

-- | Univariate polynomial from its coefficients (ordered by increasing degrees)
polyFromCoeffs :: [a] -> Polynomial a
polyFromCoeffs as = MathPol.fromCoeffs (map A as)

-- | The variable of a univariate polynomial; it is called \"soleParameter\" because 
-- this it represents the parameter of a `OneParameterSpray` spray
soleParameter :: AlgRing.C a => Polynomial a
soleParameter = polyFromCoeffs [AlgAdd.zero, AlgRing.one] 

-- | Constant rational univariate polynomial
-- 
-- >>> import Number.Ratio ( (%) )
-- >>> constQPoly (2 % 3)
--
-- prop> constQPoly (2 % 3) == qpolyFromCoeffs [2 % 3]
constQPoly :: Rational' -> QPolynomial
constQPoly = constPoly

-- | Rational univariate polynomial from coefficients
-- 
-- >>> import Number.Ratio ( (%) )
-- >>> qpolyFromCoeffs [2 % 3, 5, 7 % 4]
qpolyFromCoeffs :: [Rational'] -> QPolynomial
qpolyFromCoeffs = polyFromCoeffs

-- | The variable of a univariate rational polynomial; it is called \"qsoleParameter\" 
-- because it represents the parameter of a `OneParameterQSpray` spray 
--
-- prop> qsoleParameter == qpolyFromCoeffs [0, 1] 
qsoleParameter :: QPolynomial
qsoleParameter = qpolyFromCoeffs [0, 1] 

{- 
-- show a ratio, helper function
showQ :: (Eq a, Num a, Show a) => NumberRatio.T a -> String
showQ q = if d == 1 
  then show n 
  else show n ++ "/" ++ show d
  where
    n = NumberRatio.numerator q
    d = NumberRatio.denominator q 
 -}

-- | identify a `Polynomial a` to a `Spray a`, in order to apply the show spray 
-- functions to the univariate polynomials
polynomialToSpray :: forall a. (Eq a, AlgRing.C a) => Polynomial a -> Spray a
polynomialToSpray pol = AlgAdd.sum terms
  where
    coeffs  = MathPol.coeffs pol
    indices = findIndices (/= A AlgAdd.zero) coeffs
    get :: A a -> a
    get (A x) = x
    terms = map (\i -> get (coeffs!!i) *^ lone' 1 i) indices

qPolynomialToQSpray :: QPolynomial -> QSpray
qPolynomialToQSpray pol = AlgAdd.sum terms
  where
    coeffs  = MathPol.coeffs pol
    indices = findIndices (/= A 0) coeffs
    get :: A Rational' -> Rational
    get (A x) = NumberRatio.numerator x DR.:% NumberRatio.denominator x
    terms = map (\i -> get (coeffs!!i) *^ qlone' 1 i) indices

-- helper function; it encloses a string between two given delimiters
bracify :: (String, String) -> String -> String
bracify (lbrace, rbrace) x = lbrace ++ x ++ rbrace 

-- | helper function for prettyRatioOfPolynomials (and prettyOneParameterSpray)
showRatioOfPolynomials :: forall a. (Eq a, AlgField.C a) 
                  => (Spray a -> String) -> RatioOfPolynomials a -> String
showRatioOfPolynomials sprayShower polysRatio = 
  numeratorString ++ denominatorString
  where
    numerator         = NumberRatio.numerator polysRatio
    denominator       = NumberRatio.denominator polysRatio
    brackets          = denominator /= MathPol.const (A AlgRing.one)
    enclose = bracify ("[ ", " ]")
    numeratorString   = if brackets
      then enclose (sprayShower (polynomialToSpray numerator))
      else sprayShower (polynomialToSpray numerator)
    denominatorString = if not brackets
      then ""
      else " %//% " ++ enclose (sprayShower (polynomialToSpray denominator))

-- | Pretty form of a ratio of univariate polynomials with rational coefficients
prettyRatioOfQPolynomials ::
     String               -- ^ a string to denote the variable, e.g. @\"a\"@ 
  -> RatioOfQPolynomials 
  -> String 
prettyRatioOfQPolynomials var = showRatioOfPolynomials (prettyQSprayXYZ' [var])

-- | helper function for prettyRatioOfPolynomials (and prettyOneParameterSpray)
showQpol :: forall a. (Eq a, AlgField.C a) 
         => Polynomial a -> String -> (a -> String) -> Bool -> String
showQpol pol variable showCoeff brackets = if brackets 
  then "[ " ++ polyString ++ " ]"
  else polyString
  where
    showCoeff' :: Int -> A a -> String
    showCoeff' i (A coeff) = case i of 
      0 -> (bracify ("(", ")") . showCoeff) coeff
      _ -> if coeff == AlgRing.one 
        then "" 
        else (bracify ("(", ")") . showCoeff) coeff
    coeffs   = MathPol.coeffs pol
    nonzeros = findIndices (/= A AlgAdd.zero) coeffs
    terms    = map (pack . showTerm) nonzeros
      where
        showTerm i = case i of 
          0 -> showCoeff' 0 (coeffs !! 0)
          1 -> showCoeff' 1 (coeffs !! 1) ++ variable
          _ -> showCoeff' i (coeffs !! i) ++ variable ++ "^" ++ show i
    polyString = unpack (intercalate (pack " + ") terms)

-- | helper function for prettyRatioOfPolynomials (and prettyOneParameterSpray)
showQpolysRatio :: forall a. (Eq a, AlgField.C a) 
                   => String -> (a -> String) -> RatioOfPolynomials a -> String
showQpolysRatio var showCoeff polysRatio = numeratorString ++ denominatorString
  where
    denominator       = NumberRatio.denominator polysRatio
    brackets          = denominator /= constPoly AlgRing.one
    numeratorString   = 
      showQpol (NumberRatio.numerator polysRatio) var showCoeff brackets
    denominatorString = if not brackets
      then ""
      else " %//% " ++ showQpol denominator var showCoeff True

-- | Pretty form of a ratio of univariate polynomials
prettyRatioOfPolynomials :: (Eq a, AlgField.C a, Show a) 
  => String               -- ^ string (usually a single letter) to denote the variable, e.g. @\"a\"@
  -> RatioOfPolynomials a 
  -> String 
prettyRatioOfPolynomials var = showQpolysRatio var show 

{- -- | Pretty form of a ratio of univariate qpolynomials
prettyRatioOfQPolynomials' 
  :: String               -- ^ a string to denote the variable, e.g. @\"a\"@ 
  -> RatioOfQPolynomials 
  -> String 
prettyRatioOfQPolynomials' var = showQpolysRatio var showQ
 -}

-- | Evaluates a ratio of univariate polynomials
evalRatioOfPolynomials :: AlgField.C a 
  => a                    -- ^ the value at which the evaluation is desired
  -> RatioOfPolynomials a 
  -> a
evalRatioOfPolynomials value polysRatio = 
  resultNumerator AlgField./ resultDenominator
  where
    A resultNumerator   = 
      MathPol.evaluate (NumberRatio.numerator polysRatio) (A value)
    A resultDenominator = 
      MathPol.evaluate (NumberRatio.denominator polysRatio) (A value)


-- One-parameter sprays -------------------------------------------------------

type OneParameterSpray a = Spray (RatioOfPolynomials a)
type OneParameterQSpray  = OneParameterSpray Rational'

{- -- | simplifies a ratio of polynomials (simply by multiplying it by one)
simplifyRatioOfPolynomials :: 
  (Eq a, AlgField.C a) => RatioOfPolynomials a -> RatioOfPolynomials a
simplifyRatioOfPolynomials = (AlgRing.*) AlgRing.one

-- | Simplifies the coefficients (the fractions of univariate polynomials) of a 
-- one-parameter spray
simplifyOneParameterSpray :: 
  (Eq a, AlgField.C a) => OneParameterSpray a -> OneParameterSpray a
simplifyOneParameterSpray = HM.map simplifyRatioOfPolynomials
 -}

instance (Eq a, AlgField.C a) => AlgMod.C (Polynomial a) (OneParameterSpray a) where
  (*>) :: Polynomial a -> OneParameterSpray a -> OneParameterSpray a
  p *> spray = (p NumberRatio.:% AlgRing.one) *^ spray

instance (Eq a, AlgField.C a) => AlgRightMod.C (Polynomial a) (OneParameterSpray a) where
  (<*) :: OneParameterSpray a -> Polynomial a -> OneParameterSpray a
  spray <* p = p AlgMod.*> spray 

instance (Eq a, AlgField.C a) => AlgMod.C a (OneParameterSpray a) where
  (*>) :: a -> OneParameterSpray a -> OneParameterSpray a
  lambda *> spray = MathPol.const (A lambda) AlgMod.*> spray

instance (Eq a, AlgField.C a) => AlgRightMod.C a (OneParameterSpray a) where
  (<*) :: OneParameterSpray a -> a -> OneParameterSpray a
  spray <* lambda = lambda AlgMod.*> spray 

-- | Pretty form of a one-parameter spray, using a string (typically a letter) 
-- followed by an index to denote the variables
prettyOneParameterSprayX1X2X3 ::
     (Eq a, Show a, AlgField.C a) 
  => String              -- ^ string to denote the parameter of the spray, e.g. @\"a\"@
  -> String              -- ^ typically a letter, to denote the non-indexed variables
  -> OneParameterSpray a -- ^ a one-parameter spray; note that this function does not simplify it
  -> String 
prettyOneParameterSprayX1X2X3 a = showSprayX1X2X3 (prettyRatioOfPolynomials a) ("{ ", " }")

-- | Pretty form of a one-parameter spray, using some given strings (typically some 
-- letters) to denote the variables if possible, i.e. if enough letters are 
-- provided; otherwise this function behaves exactly like 
-- @prettyOneParameterSprayX1X2X3 a@ where @a@ is the first provided letter
prettyOneParameterSprayXYZ ::
     (Eq a, Show a, AlgField.C a) 
  => String              -- ^ string to denote the parameter of the spray, e.g. @\"a\"@
  -> [String]            -- ^ typically some letters, to denote the main variables
  -> OneParameterSpray a -- ^ a one-parameter spray; note that this function does not simplify it
  -> String 
prettyOneParameterSprayXYZ a = showSprayXYZ (prettyRatioOfPolynomials a) ("{ ", " }")

-- | Pretty form of a one-parameter spray; see the definition below and see
-- `prettyOneParameterSprayXYZ`
--
-- prop> prettyOneParameterSpray a spray == prettyOneParameterSprayXYZ a ["x","y","z"] spray
prettyOneParameterSpray ::
     (Eq a, Show a, AlgField.C a) 
  => String              -- ^ string to denote the parameter of the spray, e.g. @\"a\"@
  -> OneParameterSpray a -- ^ a one-parameter spray; note that this function does not simplify it
  -> String 
prettyOneParameterSpray a = prettyOneParameterSprayXYZ a ["x", "y", "z"]

-- | Pretty form of a one-parameter spray; see the definition below and see
-- `prettyOneParameterSprayXYZ`
--
-- prop> prettyOneParameterSpray' a spray == prettyOneParameterSprayXYZ a ["X","Y","Z"] spray
prettyOneParameterSpray' ::
     (Eq a, Show a, AlgField.C a) 
  => String              -- ^ string to denote the parameter of the spray, e.g. @\"a\"@
  -> OneParameterSpray a -- ^ a one-parameter spray; note that this function does not simplify it
  -> String 
prettyOneParameterSpray' a = prettyOneParameterSprayXYZ a ["X", "Y", "Z"]

-- | Pretty form of a one-parameter rational spray, using a string (typically a letter) 
-- followed by an index to denote the variables
prettyOneParameterQSprayX1X2X3 ::
     String          -- ^ usually a letter, to denote the parameter of the spray, e.g. @\"a\"@
  -> String          -- ^ usually a letter, to denote the non-indexed variables of the spray
  -> OneParameterQSpray  -- ^ a one-parameter rational spray; note that this function does not simplify it
  -> String 
prettyOneParameterQSprayX1X2X3 a x = 
  showSpray (prettyRatioOfQPolynomials a) ("{ ", " }") (showMonomialsX1X2X3 x)

-- | Pretty form of a one-parameter rational spray, using some given strings (typically some 
-- letters) to denote the variables if possible, i.e. if enough letters are 
-- provided; otherwise this function behaves exactly like 
-- @prettyOneParameterQSprayX1X2X3 a@ where @a@ is the first provided letter
prettyOneParameterQSprayXYZ ::
     String             -- ^ usually a letter, to denote the parameter of the spray, e.g. @\"a\"@
  -> [String]           -- ^ usually some letters, to denote the variables of the spray
  -> OneParameterQSpray -- ^ a one-parameter rational spray; note that this function does not simplify it
  -> String 
prettyOneParameterQSprayXYZ a letters = 
  showSpray (prettyRatioOfQPolynomials a) ("{ ", " }") (showMonomialsXYZ letters)

-- | Pretty form of a one-parameter rational spray, using @\"x\"@, @\"y\"@ and @\"z\"@ for the variables 
-- if possible; i.e. if the spray does not have more than three variables, otherwise 
-- @\"x1\"@, @\"x2\"@, ... are used to denote the variables
--
-- prop> prettyOneParameterQSpray a == prettyOneParameterQSprayXYZ a ["x","y","z"]
prettyOneParameterQSpray ::
     String             -- ^ usually a letter, to denote the parameter of the spray, e.g. @\"a\"@
  -> OneParameterQSpray -- ^ the one-parameter rational spray to be printed; note that this function does not simplify it
  -> String 
prettyOneParameterQSpray a = prettyOneParameterQSprayXYZ a ["x", "y", "z"] 

-- | Pretty form of a one-parameter rational spray, using @\"X\"@, @\"Y\"@ and @\"Z\"@ for the variables 
-- if possible; i.e. if the spray does not have more than three variables, otherwise 
-- @\"X1\"@, @\"X2\"@, ... are used 
--
-- prop> prettyOneParameterQSpray' a == prettyOneParameterQSprayXYZ a ["X","Y","Z"]
prettyOneParameterQSpray' ::
     String              -- ^ usually a letter, to denote the parameter of the spray, e.g. @\"a\"@
  -> OneParameterQSpray  -- ^ the one-parameter rational spray to be printed; note that this function does not simplify it
  -> String 
prettyOneParameterQSpray' a = prettyOneParameterQSprayXYZ a ["X", "Y", "Z"] 

-- | Substitutes a value to the parameter of a one-parameter spray 
-- (the variable occurring in its coefficients)
--
-- prop> evalOneParameterSpray spray x == substituteParameters spray [x]
evalOneParameterSpray :: 
  (Eq a, AlgField.C a) => OneParameterSpray a -> a -> Spray a
evalOneParameterSpray spray x = 
  removeZeroTerms $ HM.map (evalRatioOfPolynomials x) spray 

-- | Substitutes a value to the parameter of a one-parameter spray; 
-- same as `evalOneParameterSpray`
--
-- prop> substituteTheParameter spray x == substituteParameters spray [x]
substituteTheParameter :: 
  (Eq a, AlgField.C a) => OneParameterSpray a -> a -> Spray a
substituteTheParameter spray x = substituteParameters spray [x]

-- | Substitutes a value to the parameter of a one-parameter spray as well 
-- as some values to the variables of this spray
--
-- prop> evalOneParameterSpray' spray a xs == evalParametricSpray' spray [a] xs
evalOneParameterSpray' :: (Eq a, AlgField.C a) 
  => OneParameterSpray a -- ^ one-parameter spray to be evaluated
  -> a                   -- ^ a value for the parameter
  -> [a]                 -- ^ some values for the variables 
  -> a
evalOneParameterSpray' spray x xs = if length xs >= numberOfVariables spray 
  then evalSpray (evalOneParameterSpray spray x) xs
  else error "evalOneParameterSpray': not enough values provided."

-- | helper function for evalOneParameterSpray''
evalOneParameterTerm :: (Eq a, AlgField.C a) 
  => [a] -> Term (RatioOfPolynomials a) -> RatioOfPolynomials a
evalOneParameterTerm xs (powers, coeff) = 
  AlgRing.product (zipWith (AlgRing.^) xs pows) *. coeff
  where 
    pows = DF.toList (fromIntegral <$> exponents powers)

-- | Substitutes some values to the variables of a one-parameter spray; same 
-- as `evalParametricSpray`
evalOneParameterSpray'' ::
  (Eq a, AlgField.C a) => OneParameterSpray a -> [a] -> RatioOfPolynomials a
evalOneParameterSpray'' spray xs = if length xs >= numberOfVariables spray
  then AlgAdd.sum $ map (evalOneParameterTerm xs) (HM.toList spray)
  else error "evalOneParameterSpray'': not enough values provided."


-- Sprays ---------------------------------------------------------------------

data Powers = Powers
  { exponents  :: Seq Int
  , nvariables :: Int
  }
  deriving Show

instance Eq Powers where
  (==) :: Powers -> Powers -> Bool
  pows1 == pows2 = expts1' == expts2'
    where 
      (expts1', expts2') = harmonize (pows1, pows2)

instance Hashable Powers where
  hashWithSalt :: Int -> Powers -> Int
  hashWithSalt k pows = hashWithSalt k (exponents pows, nvariables pows)

-- | append trailing zeros
growSequence :: Seq Int -> Int -> Int -> Seq Int
growSequence s m n = s >< S.replicate (n - m) 0

growSequence' :: Int -> Seq Int -> Seq Int
growSequence' n s = growSequence s (S.length s) n

-- | append trailing zeros to get the same length
harmonize :: (Powers, Powers) -> (Seq Int, Seq Int)
harmonize (pows1, pows2) = (e1', e2')
 where
  e1            = exponents pows1
  e2            = exponents pows2
  n1            = nvariables pows1
  n2            = nvariables pows2
  (e1', e2') = if n1 < n2
    then (growSequence e1 n1 n2, e2)
    else (e1, growSequence e2 n2 n1)

makePowers :: Seq Int -> Powers
makePowers expnts = Powers s (S.length s)
  where 
    s = dropWhileR (== 0) expnts

type Term a = (Powers, a)
type Spray a = HashMap Powers a
type QSpray = Spray Rational
type QSpray' = Spray Rational'

instance (AlgRing.C a, Eq a) => HasVariables (Spray a) where
  type BaseRing (Spray a) = a
  --
  type VariablesType (Spray a) = Spray a
  --
  evaluate :: Spray a -> [a] -> a
  evaluate spray xyz = if length xyz >= numberOfVariables spray 
    then evalSprayHelper xyz spray
    else error "evaluate: not enough values provided."
  --
  substitute :: [Maybe a] -> Spray a -> Spray a
  substitute subs spray = if length subs >= n 
    then spray'
    else error "substitute: incorrect length of the substitutions list."
    where
      n      = numberOfVariables spray
      terms  = HM.toList spray
      spray' = sumTerms (map substituteTerm terms)
      substituteTerm :: Term a -> Term a
      substituteTerm (powers, coeff) = (powers'', coeff')
        where
          pows     = exponents powers
          nv       = nvariables powers
          indices  = findIndices isJust (take nv subs)
          pows'    = [fromIntegral (pows `index` i) | i <- indices]
          xyz      = [fromJust (subs !! i) | i <- indices]
          coeff'   = coeff AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows')
          f i a    = if i `elem` indices then 0 else a
          pows''   = S.mapWithIndex f pows
          powers'' = makePowers pows''
  --
  changeVariables :: Spray a -> [Spray a] -> Spray a
  changeVariables = composeSpray
  --
  numberOfVariables :: Spray a -> Int
  numberOfVariables spray =
    if null powers then 0 else maximum (map nvariables powers)
    where
      powers = HM.keys spray
  --
  permuteVariables :: [Int] -> Spray a -> Spray a
  permuteVariables permutation spray = 
    if isPermutation permutation && n' >= n  
      then spray'
      else error "permuteVariables: invalid permutation."
    where
      spray' = if isConstant spray
        then spray
        else HM.fromList (zip powers' coeffs)
      n  = numberOfVariables spray
      n' = maximum permutation
      isPermutation pmtn = 
        (not . null) pmtn && minimum pmtn == 1 && length (nub pmtn) == n'
      intmap         = IM.fromList (zip permutation [1 .. n'])
      invpermutation = [intmap IM.! i | i <- [1 .. n']]
      permuteSeq x   = 
        S.mapWithIndex (\i _ -> x `index` (invpermutation !! i - 1)) x 
      (powers, coeffs) = unzip (HM.toList spray)
      f pows = let expnts = (permuteSeq . growSequence' n') (exponents pows) in
                   makePowers expnts
      powers' = map f powers
  --
  swapVariables :: (Int, Int) -> Spray a -> Spray a
  swapVariables (i, j) spray = 
    if i>=1 && j>=1  
      then spray'
      else error "swapVariables: invalid indices."
    where
      spray' = if isConstant spray
        then spray
        else HM.fromList (zip powers' coeffs)
      n = maximum [numberOfVariables spray, i, j]
      f k | k == i    = j
          | k == j    = i
          | otherwise = k
      transposition = map f [1 .. n]
      permuteSeq x  = 
        S.mapWithIndex (\ii _ -> x `index` (transposition !! ii - 1)) x 
      (powers, coeffs) = unzip (HM.toList spray)
      g pows = let expnts = (permuteSeq . growSequence' n) (exponents pows) in
                 makePowers expnts
      powers' = map g powers
  --
  derivative :: Int -> Spray a -> Spray a 
  derivative i p = if i >= 1 
    then removeZeroTerms $ HM.fromListWith (AlgAdd.+) terms
    else error "derivative: invalid index."
    where
      terms = [ derivTerm term | term <- HM.toList p ]
      derivTerm :: Term a -> Term a 
      derivTerm (pows, coef) = if i' >= S.length expts 
        then (Powers S.empty 0, AlgAdd.zero)
        else (pows', coef')
        where
          i'     = i - 1
          expts  = exponents pows
          expt_i = expts `index` i'
          expts' = adjust (subtract 1) i' expts
          coef'  = expt_i .^ coef
          pows'  = makePowers expts' 

-- | addition of two sprays
addSprays :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
addSprays p q = removeZeroTerms $ HM.unionWith (AlgAdd.+) p q -- HM.foldlWithKey' f p q
--  where 
--    f s powers coef = HM.insertWith (AlgAdd.+) powers coef s

-- | addition of a term to a spray
addTerm :: (AlgAdd.C a, Eq a) => Spray a -> Term a -> Spray a
addTerm spray (powers, coeff) = 
  if getCoefficient' powers spray AlgAdd.+ coeff == AlgAdd.zero
    then 
      HM.delete powers spray
    else
      HM.insertWith (AlgAdd.+) powers coeff spray

-- | sum list of terms
sumTerms :: (Eq a, AlgAdd.C a) => [Term a] -> Spray a
sumTerms = removeZeroTerms . HM.fromListWith (AlgAdd.+) 

-- | opposite spray
negateSpray :: AlgAdd.C a => Spray a -> Spray a
negateSpray = HM.map AlgAdd.negate

-- | scale a spray by a scalar
scaleSpray :: (AlgRing.C a, Eq a) => a -> Spray a -> Spray a
scaleSpray lambda p = removeZeroTerms $ HM.map (lambda AlgRing.*) p

-- | multiply two terms
multTerm :: AlgRing.C a => Term a -> Term a -> Term a
multTerm (pows1, coef1) (pows2, coef2) = (pows, coef1 AlgRing.* coef2)
 where
  (expts1', expts2') = harmonize (pows1, pows2)
  expts              = S.zipWith (+) expts1' expts2'
  pows               = makePowers expts

-- | multiply a spray by a term
multSprayByTerm :: (Eq a, AlgRing.C a) => Spray a -> Term a -> Spray a
multSprayByTerm spray term = removeZeroTerms $ HM.fromList prods
  where
    prods = [multTerm trm term | trm <- HM.toList spray]

-- | multiply two sprays
multSprays :: (AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
multSprays p q = removeZeroTerms $ HM.fromListWith (AlgAdd.+) prods
 where
  p'    = HM.toList p
  q'    = HM.toList q
  prods = [ multTerm mp mq | mp <- p', mq <- q' ]

instance (AlgAdd.C a, Eq a) => AlgAdd.C (Spray a) where
  (+) :: Spray a -> Spray a -> Spray a
  p + q  = addSprays p q
  zero :: Spray a
  zero   = HM.empty
  negate :: Spray a -> Spray a
  negate = negateSpray

instance (AlgRing.C a, Eq a) => AlgMod.C a (Spray a) where
  (*>) :: a -> Spray a -> Spray a
  lambda *> p = scaleSpray lambda p

instance (AlgRing.C a, Eq a) => AlgRightMod.C a (Spray a) where
  (<*) :: Spray a -> a -> Spray a
  p <* lambda = scaleSpray lambda p

instance (AlgRing.C a, Eq a) => AlgRing.C (Spray a) where
  (*) :: Spray a -> Spray a -> Spray a
  p * q = multSprays p q
  one :: Spray a
  one = lone 0

{- instance (AlgRing.C a, Eq a) => Num (Spray a) where
  p + q = addSprays p q
  negate = negateSpray
  p * q = multSprays p q
  fromInteger n = fromInteger n .^ AlgRing.one
  abs _ = error "Prelude.Num.abs: inappropriate abstraction"
  signum _ = error "Prelude.Num.signum: inappropriate abstraction"
 -} 

infixl 6 ^+^
-- | Addition of two sprays
(^+^) :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
(^+^) p q = p AlgAdd.+ q

infixl 6 ^-^
-- | Substraction of two sprays
(^-^) :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
(^-^) p q = p AlgAdd.- q

infixl 7 ^*^
-- | Multiply two sprays
(^*^) :: (AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
(^*^) p q = p AlgRing.* q

infixr 8 ^**^
-- | Power of a spray
(^**^) :: (AlgRing.C a, Eq a) => Spray a -> Int -> Spray a
(^**^) p n = if n >= 0 
  then p AlgRing.^ fromIntegral n
  else error "(^**^): negative power of a spray is not allowed."

infixr 7 *^
-- | Scales a spray by a scalar; if you import the /Algebra.Module/ module 
-- then it is the same operation as @(*>)@ from this module
(*^) :: (AlgRing.C a, Eq a) => a -> Spray a -> Spray a
(*^) lambda pol = lambda AlgMod.*> pol

infixr 7 /^
-- | Divides a spray by a scalar; you can equivalently use `(/>)` if the type 
-- of the scalar is not ambiguous
(/^) :: (AlgField.C a, Eq a) => Spray a -> a -> Spray a
(/^) spray lambda = spray /> lambda

-- | remove zero terms of a spray
removeZeroTerms :: (AlgAdd.C a, Eq a) => Spray a -> Spray a
removeZeroTerms = HM.filter (/= AlgAdd.zero)

-- | helper function for lone and lone'
lonePower :: Int -> Int -> Powers
lonePower n p = if n == 0 
  then Powers S.empty 0
  else Powers (S.replicate (n - 1) 0 |> p) n

-- | The @n@-th polynomial variable @x_n@ as a spray; one usually builds a 
-- spray by introducing these variables and combining them with the arithmetic 
-- operations
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> spray = 2*^x^**^2 ^-^ 3*^y
-- >>> putStrLn $ prettyNumSpray spray
-- 2*x^2 - 3*y
--
-- prop> lone 0 == unitSpray
lone :: AlgRing.C a => Int -> Spray a
lone n = if n >= 0 
  then HM.singleton (lonePower n 1) AlgRing.one
  else error "lone: invalid index."

-- | The @n@-th polynomial variable for rational sprays; this is just a 
-- specialization of `lone`
qlone :: Int -> QSpray
qlone = lone

-- | The spray @x_n^p@; more efficient than exponentiating @lone n@
--
-- prop> lone' 2 10 = lone 2 ^**^ 10
lone' :: 
     AlgRing.C a 
  => Int     -- ^ index 
  -> Int     -- ^ exponent
  -> Spray a
lone' n p 
  | n < 0     = error "lone': invalid index."
  | p < 0     = error "lone': invalid exponent"
  | otherwise = HM.singleton (lonePower n p) AlgRing.one

-- | The rational spray @x_n^p@
qlone' :: 
     Int     -- ^ index 
  -> Int     -- ^ exponent
  -> QSpray
qlone' = lone'

loneTerm' :: AlgRing.C a => Int -> Int -> Term a
loneTerm' n p = (lonePower n p, AlgRing.one)

-- | Monomial spray, e.g. @monomial [(1,4),(3,2)]@ is @x^4.z^2@; indices 
-- and exponents must be positive but this is not checked
-- prop> monomial [(1, 4), (3, 2)] == (lone 1 ^**^ 4) ^*^ (lone 3 ^**^ 2)
monomial :: 
     AlgRing.C a
  => [(Int, Int)] -- ^ list of (index, exponent); duplicates are deleted
  -> Spray a
monomial nps = if null nps 
  then unitSpray 
  else HM.singleton (Powers expnts (S.length expnts)) AlgRing.one
  where 
    nps' = nub nps
    nv = maximum (map fst nps')
    expnts = S.fromList $ map (\i -> fromMaybe 0 (lookup i nps')) [1 .. nv]

-- | Monomial rational spray, a specialization of 'monomial'
--
-- prop> qmonomial [(1, 4), (3, 2)] == (qlone 1 ^**^ 4) ^*^ (qlone 3 ^**^ 2)
qmonomial :: 
     [(Int, Int)]
  -> QSpray
qmonomial = monomial

-- | The unit spray
--
-- prop> spray ^*^ unitSpray == spray
unitSpray :: AlgRing.C a => Spray a
unitSpray = HM.singleton (Powers S.empty 0) AlgRing.one

-- | The null spray
--
-- prop> spray ^+^ zeroSpray == spray
zeroSpray :: (Eq a, AlgAdd.C a) => Spray a
zeroSpray = AlgAdd.zero

-- | whether the spray is zero
isZeroSpray :: Spray a -> Bool
isZeroSpray = HM.null 

-- | Constant spray
--
-- prop> constantSpray 3 == 3 *^ unitSpray
constantSpray :: (Eq a, AlgAdd.C a) => a -> Spray a
constantSpray c = if c == AlgAdd.zero 
  then HM.empty 
  else HM.singleton (Powers S.empty 0) c

-- | Get coefficient of a term of a spray 
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> p = 2 *^ (2 *^ (x^**^3 ^*^ y^**^2)) ^+^ 4*^z ^+^ 5*^unitSpray
-- >>> getCoefficient [3, 2, 0] p
-- 4
-- >>> getCoefficient [0, 4] p
-- 0
getCoefficient :: AlgAdd.C a => [Int] -> Spray a -> a
getCoefficient expnts = getCoefficient' powers
  where
    powers = makePowers (S.fromList expnts)
--    expnts' = S.dropWhileR (== 0) (S.fromList expnts)
--    powers  = Powers expnts' (S.length expnts')

getCoefficient' :: AlgAdd.C a => Powers -> Spray a -> a
getCoefficient' powers spray = fromMaybe AlgAdd.zero (HM.lookup powers spray)

-- | Get the constant term of a spray
--
-- prop> getConstantTerm p == getCoefficient [] p 
getConstantTerm :: AlgAdd.C a => Spray a -> a
getConstantTerm = getCoefficient' (Powers S.empty 0)

-- | remove the constant term of a spray
removeConstantTerm :: Spray a -> Spray a
removeConstantTerm = HM.delete (Powers S.empty 0)

-- | Whether a spray is constant; same as `isConstant`
isConstantSpray :: (Eq a, AlgRing.C a) => Spray a -> Bool
isConstantSpray = isConstant

-- | helper function to unify evalSpray and evalSpraySpray
evalSprayHelper :: forall a. AlgRing.C a => [a] -> Spray a -> a
evalSprayHelper xyz spray = 
  AlgAdd.sum $ map evalTerm (HM.toList spray)
  where
    evalTerm :: Term a -> a
    evalTerm (powers, coeff) = 
      coeff AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows)
      where 
        pows = DF.toList (fromIntegral <$> exponents powers)

-- | Evaluates a spray; same as `evaluate`
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> spray = 2*^x^**^2 ^-^ 3*^y
-- >>> evalSpray spray [2, 1]
-- 5
evalSpray :: (Eq a, AlgRing.C a) => Spray a -> [a] -> a
evalSpray = evaluate

-- | Evaluates the coefficients of a spray with spray coefficients; 
-- same as `substituteParameters`
evalSpraySpray :: (Eq a, AlgRing.C a) => Spray (Spray a) -> [a] -> Spray a
evalSpraySpray spray xyz = if length xyz >= n 
  then HM.map (evalSprayHelper xyz) spray
  else error "evalSpraySpray: not enough values provided."
    where 
      n = maximum (HM.elems $ HM.map numberOfVariables spray)

-- | spray from term
fromTerm :: Term a -> Spray a
fromTerm (pows, coeff) = HM.singleton pows coeff

-- | Substitutes some values to some variables of a spray; same as `substitute`
--
-- >>> x1 = lone 1 :: Spray Int
-- >>> x2 = lone 2 :: Spray Int
-- >>> x3 = lone 3 :: Spray Int
-- >>> p = x1^**^2 ^-^ x2 ^+^ x3 ^-^ unitSpray
-- >>> p' = substituteSpray [Just 2, Nothing, Just 3] p
-- >>> putStrLn $ prettyNumSprayX1X2X3 "x" p'
-- -x2 + 6 
substituteSpray :: (Eq a, AlgRing.C a) => [Maybe a] -> Spray a -> Spray a
substituteSpray = substitute 

-- | Converts a spray with rational coefficients to a spray with double 
-- coefficients (useful for evaluation)
fromRationalSpray :: Spray Rational -> Spray Double
fromRationalSpray = HM.map fromRational

-- | Sustitutes the variables of a spray with some sprays; same as `changeVariables`
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> p = x ^+^ y
-- >>> q = composeSpray p [x ^+^ y ^+^ z, z]
-- >>> putStrLn $ prettyNumSpray' q
-- X + Y + 2*Z
composeSpray :: 
  forall a. (AlgRing.C a, Eq a) => Spray a -> [Spray a] -> Spray a
composeSpray p = evalSpray (identify p)
  where 
    identify :: Spray a -> Spray (Spray a)
    identify = HM.map constantSpray

-- | Creates a spray from a list of terms
fromList :: 
  (AlgRing.C a, Eq a)
  => [([Int], a)]        -- ^ list of (exponents, coefficient)
  -> Spray a
fromList x = removeZeroTerms $ HM.fromListWith (AlgAdd.+) $ map
              (first (makePowers . S.fromList)) x


-- pretty stuff ---------------------------------------------------------------

-- | Prints a spray; this function is exported for possible usage in other packages
showSpray ::
     (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String)        -- ^ pair of braces to enclose the coefficients
  -> ([Seq Int] -> [String]) -- ^ function mapping a list of exponents to a list of strings representing the monomials corresponding to these exponents
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSpray showCoef braces showMonomials spray = 
  if isZeroSpray spray 
    then "0"
    else unpack $ intercalate (pack " + ") stringTerms
  where
    terms = sortBy (flip compare `on` fexpts) (HM.toList spray)
    fexpts term = exponents $ fst term
    coeffs = map snd terms
    powers = map (exponents . fst) terms
    stringMonomials = showMonomials powers
    stringTerms = zipWith f coeffs stringMonomials
    f coeff smonomial 
      | smonomial == "" = pack scoeff'
      | scoeff == ""    = pack smonomial
      | otherwise       = pack $ scoeff' ++ "*" ++ smonomial
      where
        scoeff  = showCoef coeff
        scoeff' = bracify braces scoeff 

-- | Prints a spray, with monomials shown as "x.z^2", and with 
-- a user-defined showing function for the coefficients
showSprayXYZ ::
     (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String)        -- ^ used to enclose the coefficients, usually a pair of braces
  -> [String]                -- ^ typically some letters, to print the variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSprayXYZ showCoef braces letters spray =
  if null letters
    then error "showSprayXYZ: empty list of strings."
    else showSpray showCoef braces (showMonomialsXYZ letters) spray

-- | Prints a spray, with monomials shown as @\"x.z^2\"@, and with 
-- a user-defined showing function for the coefficients; this is the same as 
-- the function `showSprayXYZ` with the pair of braces @("(", ")")@
showSprayXYZ' ::
     (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> [String]                -- ^ typically some letters, to print the variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSprayXYZ' showCoef = showSprayXYZ showCoef ("(", ")")

-- | Pretty form of a spray with monomials displayed in the style of @\"x.z^2\"@; 
-- you should rather use `prettyNumSprayXYZ` or `prettyQSprayXYZ` if your 
-- coefficients are numeric
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySprayXYZ ["X", "Y", "Z"] p
-- (2)*X + (3)*Y^2 + (-4)*Z^3
-- >>> putStrLn $ prettySprayXYZ ["X", "Y"] p
-- (2)*X1 + (3)*X2^2 + (-4)*X3^3
prettySprayXYZ :: 
     (Show a) 
  => [String]                -- ^ typically some letters, to print the variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
prettySprayXYZ = showSprayXYZ' show
  
-- | Pretty form of a spray, with monomials shown as "x1.x3^2", and with 
-- a user-defined showing function for the coefficients
showSprayX1X2X3 ::
     (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String)        -- ^ used to enclose the coefficients
  -> String                  -- ^ typically a letter, to print the non-indexed variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSprayX1X2X3 showCoef braces letter =
  showSpray showCoef braces (showMonomialsX1X2X3 letter)

-- | Pretty form of a spray, with monomials shown as "x1.x3^2", and with 
-- a user-defined showing function for the coefficients; this is the same as 
-- the function `showSprayX1X2X3` with the pair of braces @("(", ")")@ used to 
-- enclose the coefficients
showSprayX1X2X3' ::
     (a -> String)           -- ^ function mapping a coefficient to a string, e.g. 'show'
  -> String                  -- ^ typically a letter, to print the non-indexed variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSprayX1X2X3' showCoef = showSprayX1X2X3 showCoef ("(", ")")

-- | Pretty form of a spray with monomials displayed in the style of @\"x1.x3^2\"@; 
-- you should rather use `prettyNumSprayX1X2X3` or `prettyQSprayX1X2X3` if your 
-- coefficients are numeric
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> spray = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySprayX1X2X3 "X" spray
-- (2)*X1 + (3)*X2^2 + (-4)*X3^3
prettySprayX1X2X3 :: 
     Show a 
  => String                -- ^ typically a letter, to print the non-indexed variables
  -> Spray a               -- ^ the spray to be printed
  -> String
prettySprayX1X2X3 = showSprayX1X2X3' show

-- | Pretty form of a spray with monomials displayed in the style of @\"x.z^2\"@; 
-- you should rather use `prettyNumSpray` or `prettyQSpray` if you deal with 
-- sprays with numeric coefficients
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray p
-- (2)*x + (3)*y^2 + (-4)*z^3
-- >>> putStrLn $ prettySpray (p ^+^ lone 4)
-- (2)*x1 + (3)*x2^2 + (-4)*x3^3 + x4
--
-- prop> prettySpray spray == prettySprayXYZ ["x", "y", "z"] spray
prettySpray :: (Show a) => Spray a -> String
prettySpray = prettySprayXYZ ["x", "y", "z"]

-- | Pretty form of a spray, with monomials shown as @\"x1.x3^2\"@; use 
-- `prettySprayX1X2X3` to change the letter (or `prettyNumSprayX1X2X3` 
-- or `prettyQSprayX1X2X3` if the coefficients are numeric)
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray' p
-- (2)*x1 + (3)*x2^2 + (-4)*x3^3 
prettySpray' :: Show a => Spray a -> String
prettySpray' = prettySprayX1X2X3 "x"

-- | showMonomialOld "x" [0, 2, 1] = x^(0, 2, 1)
showMonomialsOld :: String -> [Seq Int] -> [String]
showMonomialsOld var = map (showMonomialOld var) 
  where
    showMonomialOld :: String -> Seq Int -> String
    showMonomialOld a pows = 
      unpack $ append (pack x) (cons '(' $ snoc string ')')
      where
        x      = a ++ "^"
        string = intercalate (pack ", ") (map (pack . show) (DF.toList pows))

-- | Pretty form of a spray; you will probably prefer `prettySpray` or `prettySpray'`
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray'' "x" p
-- (2)*x^(1) + (3)*x^(0, 2) + (-4)*x^(0, 0, 3)
prettySpray'' ::
     Show a 
  => String        -- ^ a string denoting the variables, e.g. \"x\"
  -> Spray a       -- ^ the spray
  -> String
prettySpray'' var = showSpray show ("(", ")") (showMonomialsOld var)

-- | Show a spray with numeric coefficients; this function is exported for 
-- possible usage in other packages
showNumSpray :: 
     (Num a, Ord a)
  => ([Seq Int] -> [String]) -- ^ function mapping a list of monomial exponents to a list of strings representing the monomials
  -> (a -> String)           -- ^ function mapping a positive coefficient to a string
  -> Spray a
  -> String
showNumSpray showMonomials showCoeff spray = 
  if isZeroSpray spray 
    then "0" 
    else concat $ zipWith (++) stringSigns stringTerms
  where
    terms = sortBy (flip compare `on` (exponents . fst)) (HM.toList spray)
    coeffs = map snd terms
    (firstCoeff, otherCoeffs) = fromJust (uncons coeffs)
    firstSign   = if firstCoeff > 0 then "" else "-"
    otherSigns  = map (\x -> if x > 0 then " + " else " - ") otherCoeffs
    stringSigns = firstSign : otherSigns
    absCoeffs = map abs coeffs
    powers = map (exponents . fst) terms
    stringMonomials = showMonomials powers
    stringTerms = zipWith f absCoeffs stringMonomials
    f acoeff smonomial 
      | smonomial == "" = showCoeff acoeff
      | scoeff == ""    = smonomial
      | otherwise       = scoeff ++ "*" ++ smonomial
      where
        scoeff = if acoeff == 1 then "" else showCoeff acoeff

-- | showMonomialX1X2X3 "X" [0, 2, 1] = "X2^2.X3"
showMonomialX1X2X3 :: String -> Seq Int -> Text
showMonomialX1X2X3 x pows = x1x2x3
 where
  f i p 
    | p == 0    = pack ""
    | p == 1    = pack $ x ++ show i
    | otherwise = pack $ x ++ show i ++ "^" ++ show p
  indices = S.findIndicesL (/= 0) pows
  x1x2x3 = 
    intercalate (pack ".") (map (\i -> f (i+1) (pows `index` i)) indices)

-- | showMonomialsX1X2X3 "X" [[0, 2, 1], [1, 2]] = ["X2^2.X3", "X1.X2"]
showMonomialsX1X2X3 :: String -> [Seq Int] -> [String]
showMonomialsX1X2X3 x = map (unpack . showMonomialX1X2X3 x)

-- | showMonomialXYZ ["X", "Y", "Z"] 3 [1, 2, 1] = X.Y^2.Z
--   showMonomialXYZ ["X", "Y", "Z"] 3 [1, 2, 1, 2] = X1.X2^2.X3.X4^2
showMonomialXYZ :: [String] -> Int -> Seq Int -> Text
showMonomialXYZ letters n pows = if n <= length letters
  then xyz
  else showMonomialX1X2X3 (letters !! 0) pows
 where
  f letter p 
    | p == 0    = pack ""
    | p == 1    = pack letter
    | otherwise = pack $ letter ++ "^" ++ show p
  indices = S.findIndicesL (/= 0) pows
  xyz = intercalate (pack ".") 
        (map (\i -> f (letters!!i) (pows `index` i)) indices)

-- | showMonomialsXYZ ["X", "Y", "Z"] [[0, 2, 1], [1, 2]] = ["Y^2.Z", "X.Y^2"]
showMonomialsXYZ :: [String] -> [Seq Int] -> [String]
showMonomialsXYZ letters powers = map (unpack . showMonomialXYZ letters n) powers
  where 
    n = maximum (map S.length powers)

-- | Pretty form of a spray with numeric coefficients, printing monomials as @\"x1.x3^2\"@
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettyNumSprayX1X2X3 "x" p
-- 2*x1 + 3*x2^2 - 4*x3^3 
prettyNumSprayX1X2X3 :: (Num a, Ord a, Show a)
  => String   -- ^ usually a letter such as @\"x\"@ to denote the non-indexed variables
  -> Spray a
  -> String
prettyNumSprayX1X2X3 x = showNumSpray (showMonomialsX1X2X3 x) show

-- | Pretty form of a spray with numeric coefficients, printing monomials as @\"x.z^2\"@
-- if possible, i.e. if enough letters are provided, otherwise as @\"x1.x3^2\"@
--
-- >>> x = lone 1 :: Spray Int
-- >>> y = lone 2 :: Spray Int
-- >>> z = lone 3 :: Spray Int
-- >>> w = lone 4 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettyNumSprayXYZ ["x","y","z"] p
-- 2*x + 3*y^2 - 4*z^3 
-- >>> putStrLn $ prettyNumSprayXYZ ["x","y","z"] (p ^+^ w)
-- 2*x1 + 3*x2^2 - 4*x3^3 + x4
-- >>> putStrLn $ prettyNumSprayXYZ ["a","b","c"] (p ^+^ w)
-- 2*a1 + 3*a2^2 - 4*a3^3 + a4
prettyNumSprayXYZ :: (Num a, Ord a, Show a)
  => [String] -- ^ usually some letters, denoting the variables
  -> Spray a
  -> String
prettyNumSprayXYZ letters = showNumSpray (showMonomialsXYZ letters) show

-- | helper function for showQSpray
showRatio :: Rational -> String
showRatio q = if d == 1 
  then show n 
  else "(" ++ show n ++ "/" ++ show d ++ ")"
  where
    n = DR.numerator q
    d = DR.denominator q 

-- | helper function for showQSpray' 
showRatio' :: (Eq a, Num a, Show a) => NumberRatio.T a -> String
showRatio' q = if d == 1 
  then show n 
  else "(" ++ show n ++ "/" ++ show d ++ ")"
  where
    n = NumberRatio.numerator q
    d = NumberRatio.denominator q 

-- | Prints a `QSpray`; for internal usage but exported for usage in other packages
showQSpray :: 
   ([Seq Int] -> [String]) -- ^ function printing monomials
  -> QSpray
  -> String
showQSpray showMonomials = showNumSpray showMonomials showRatio

-- | Prints a `QSpray'`; for internal usage but exported for usage in other packages
showQSpray' :: 
   ([Seq Int] -> [String]) -- ^ function mapping a list of monomials exponents to a list of strings
  -> QSpray'
  -> String
showQSpray' showMonomials = showNumSpray showMonomials showRatio'

-- | Pretty form of a spray with rational coefficients, printing monomials in 
-- the style of @\"x1.x3^2\"@
--
-- >>> x = lone 1 :: QSpray
-- >>> y = lone 2 :: QSpray
-- >>> z = lone 3 :: QSpray
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ (4%3)*^z^**^3
-- >>> putStrLn $ prettyQSprayX1X2X3 "x" p
-- 2*x1 + 3*x2^2 - (4/3)*x3^3 
prettyQSprayX1X2X3 :: 
     String   -- ^ usually a letter such as @\"x\"@, to denote the non-indexed variables
  -> QSpray
  -> String
prettyQSprayX1X2X3 x = showQSpray (showMonomialsX1X2X3 x)

-- | Same as `prettyQSprayX1X2X3` but for a `QSpray'` spray
prettyQSprayX1X2X3' :: 
     String   -- ^ usually a letter such as @\"x\"@, to denote the non-indexed variables
  -> QSpray'
  -> String
prettyQSprayX1X2X3' x = showQSpray' (showMonomialsX1X2X3 x)

-- | Pretty form of a spray with rational coefficients, printing monomials in 
-- the style of @\"x.z^2\"@ with the provided letters if possible, i.e. if enough 
-- letters are provided, otherwise in the style @\"x1.x3^2\"@, taking the first 
-- provided letter to denote the non-indexed variables
--
-- >>> x = lone 1 :: QSpray
-- >>> y = lone 2 :: QSpray
-- >>> z = lone 3 :: QSpray
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ (4%3)*^z^**^3
-- >>> putStrLn $ prettyQSprayXYZ ["x","y","z"] p
-- 2*x + 3*y^2 - (4/3)*z^3 
-- >>> putStrLn $ prettyQSprayXYZ ["x","y"] p
-- 2*x1 + 3*x2^2 - (4%3)*x3^3
-- >>> putStrLn $ prettyQSprayXYZ ["a","b"] p
-- 2*a1 + 3*a2^2 - (4/3)*a3^3
prettyQSprayXYZ :: 
    [String]   -- ^ usually some letters, to denote the variables
  -> QSpray
  -> String
prettyQSprayXYZ letters = showQSpray (showMonomialsXYZ letters)

-- | Same as `prettyQSprayXYZ` but for a `QSpray'` spray
prettyQSprayXYZ' :: 
    [String]   -- ^ usually some letters, to denote the variables
  -> QSpray'
  -> String
prettyQSprayXYZ' letters = showQSpray' (showMonomialsXYZ letters)

-- | Pretty printing of a spray with rational coefficients
-- prop> prettyQSpray == prettyQSprayXYZ ["x", "y", "z"]
prettyQSpray :: QSpray -> String
prettyQSpray = prettyQSprayXYZ ["x", "y", "z"]

-- | Pretty printing of a spray with rational coefficients
-- prop> prettyQSpray'' == prettyQSprayXYZ ["X", "Y", "Z"]
prettyQSpray'' :: QSpray -> String
prettyQSpray'' = prettyQSprayXYZ ["X", "Y", "Z"]

-- | Pretty printing of a spray with rational coefficients
-- prop> prettyQSpray' == prettyQSprayXYZ' ["x", "y", "z"]
prettyQSpray' :: QSpray' -> String
prettyQSpray' = prettyQSprayXYZ' ["x", "y", "z"]

-- | Pretty printing of a spray with rational coefficients
-- prop> prettyQSpray''' == prettyQSprayXYZ' ["X", "Y", "Z"]
prettyQSpray''' :: QSpray' -> String
prettyQSpray''' = prettyQSprayXYZ' ["X", "Y", "Z"]

-- | Pretty printing of a spray with numeric coefficients
-- prop> prettyNumSpray == prettyNumSprayXYZ ["x", "y", "z"]
prettyNumSpray :: (Num a, Ord a, Show a) => Spray a -> String
prettyNumSpray = prettyNumSprayXYZ ["x", "y", "z"]

-- | Pretty printing of a spray with numeric coefficients
-- prop> prettyNumSpray' == prettyNumSprayXYZ ["X", "Y", "Z"]
prettyNumSpray' :: (Num a, Ord a, Show a) => Spray a -> String
prettyNumSpray' = prettyNumSprayXYZ ["X", "Y", "Z"]


-- misc -----------------------------------------------------------------------

-- | Terms of a spray
sprayTerms :: Spray a -> HashMap (Seq Int) a
sprayTerms = HM.mapKeys exponents

-- | Spray as a list
toList :: Spray a -> [([Int], a)]
toList p = HM.toList $ HM.mapKeys (DF.toList . exponents) p

-- | Bombieri spray (for internal usage in the \'scubature\' library)
bombieriSpray :: (Eq a, AlgAdd.C a) => Spray a -> Spray a
bombieriSpray = HM.mapWithKey f
 where
  f pows          = times (pfactorial $ exponents pows)
  pfactorial pows = product $ DF.toList $ factorial <$> S.filter (/= 0) pows
  factorial n     = product [1 .. n]
  times k x       = k .^ x 

-- | Whether two sprays are equal up to a scalar factor
collinearSprays :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> Bool
collinearSprays spray1 spray2 = 
  isZeroSpray spray1 && isZeroSpray spray2 ||
    (not . isZeroSpray) spray1 && (not . isZeroSpray) spray2 &&
      snd (leadingTerm spray1) *^ spray2 == snd (leadingTerm spray2) *^ spray1


-- division stuff -------------------------------------------------------------

-- | index of the maximum of a list
-- maxWithIndex :: Ord a => [a] -> (Int, a)
-- maxWithIndex = maximumBy (comparing snd) . zip [0 .. ]

-- | Leading term of a spray 
leadingTerm :: Spray a -> Term a
leadingTerm p = (biggest, p HM.! biggest) 
  where
    powers  = HM.keys p
    biggest = maximumBy (comparing exponents) powers
--    (i, biggest) = maxWithIndex powers
    -- biggest = powers !! i

-- | whether a term divides another term
divides :: Term a -> Term a -> Bool
divides (powsP, _) (powsQ, _) = nvP <= nvQ && lower
  where
    nvP = nvariables powsP
    nvQ = nvariables powsQ
    expntsP = exponents powsP
    expntsQ = exponents powsQ
    lower   = DF.all (uncurry (<=)) (S.zip expntsP expntsQ)

-- | quotient of term Q by term p, assuming P divides Q
quotient :: AlgField.C a => Term a -> Term a -> Term a
quotient (powsQ, coeffQ) (powsP, coeffP) = (pows, coeff)
  where
    (expntsP, expntsQ) = harmonize (powsP, powsQ)
    expnts             = S.zipWith (-) expntsQ expntsP
    pows               = makePowers expnts
    coeff              = coeffQ AlgField./ coeffP

-- | Remainder of the division of a spray by a list of divisors, 
-- using the lexicographic ordering of the monomials
sprayDivisionRemainder :: forall a. (Eq a, AlgField.C a) 
                          => Spray a -> [Spray a] -> Spray a
sprayDivisionRemainder p qs = 
  if n == 0 
    then error "sprayDivisionRemainder: the list of divisors is empty." 
    else ogo p zeroSpray
  where
    n = length qs
    qsltqs = zip qs (map leadingTerm qs)
    g :: Term a -> Spray a -> Spray a -> (Spray a, Spray a)
    g lts s r = (HM.delete (fst lts) s, addTerm r lts)
    go :: Term a -> Spray a -> Spray a -> Int -> Bool -> (Spray a, Spray a)
    go lts !s r !i !divoccured
      | divoccured = (s, r)
      | i == n     = g lts s r 
      | otherwise  = go lts news r (i+1) newdivoccured
        where
          (q, ltq)      = qsltqs !! i
          newdivoccured = divides ltq lts
          news          = if newdivoccured
            then s ^-^ multSprayByTerm q (quotient lts ltq)
            else s
    ogo :: Spray a -> Spray a -> Spray a
    ogo !s !r 
      | isZeroSpray s    = r
      | otherwise        = ogo s' r'
        where
          (s', r') = go (leadingTerm s) s r 0 False

-- | Division of a spray by a spray
sprayDivision :: 
  (Eq a, AlgField.C a) 
  => Spray a            -- ^ dividand 
  -> Spray a            -- ^ divisor
  -> (Spray a, Spray a) -- ^ (quotient, remainder)
sprayDivision sprayA sprayB =
  if isConstant sprayB
    then if isZeroSpray sprayB
      then 
        error "sprayDivision: division by zero."
      else 
        let c = getConstantTerm sprayB in (sprayA /> c, zeroSpray)
    else sprayDivision0 sprayA sprayB

sprayDivision0 :: forall a. (Eq a, AlgField.C a) 
  => Spray a            -- ^ dividand 
  -> Spray a            -- ^ divisor
  -> (Spray a, Spray a) -- ^ (quotient, remainder)
sprayDivision0 sprayA sprayB =
  ogo sprayA zeroSpray zeroSpray
  where
    ltB = leadingTerm sprayB
    ogo :: Spray a -> Spray a -> Spray a -> (Spray a, Spray a)
    ogo !p !q !r 
      | isZeroSpray p    = (q, r)
      | otherwise        = ogo p' q' r'
        where
          ltp = leadingTerm p
          (p', q', r') = if divides ltB ltp
            then (newp, newq, r)
            else (HM.delete (fst ltp) p, q, addTerm r ltp)
          qtnt  = quotient ltp ltB
          newp = p ^-^ multSprayByTerm sprayB qtnt
          newq = addTerm q qtnt

-- | division of univariate sprays with degree(dividend) >= degree(divisor)
univariateSprayDivision :: forall a. (Eq a, AlgField.C a) 
  => Spray a            -- ^ dividand 
  -> Spray a            -- ^ divisor
  -> (Spray a, Spray a) -- ^ (quotient, remainder)
univariateSprayDivision sprayA sprayB =
  if isConstant sprayB 
    then 
      let c = getConstantTerm sprayB in (sprayA /> c, zeroSpray)
    else 
      ogo sprayA zeroSpray zeroSpray
  where
    (powsLTB, coeffLTB) = leadingTerm sprayB
    degB = exponents powsLTB
    expntLTB = degB `index` 0
    ogo :: Spray a -> Spray a -> Spray a -> (Spray a, Spray a)
    ogo !p !q !r
      | isZeroSpray p = (q, r)
      | otherwise     = ogo p' q' r'
        where
          (powsLTP, coeffLTP)  = leadingTerm p
          degP = exponents powsLTP
          (p', q', r') = if degB <= degP
            then (newp, newq, r)
            else (zeroSpray, q, r ^+^ p)
          newp = p ^-^ multSprayByTerm sprayB qtnt
          newq = addTerm q qtnt
          qtnt = (pows, coeff)
          expntLTP = degP `index` 0
          pows = if expntLTP == expntLTB
            then Powers S.empty 0 
            else Powers (S.singleton (expntLTP - expntLTB)) 1
          coeff = coeffLTP AlgField./ coeffLTB


-- Groebner stuff -------------------------------------------------------------

-- | slight modification of `sprayDivisionRemainder` to speed up groebner00
sprayDivisionRemainder' ::
     forall a. (Eq a, AlgField.C a) 
  => Spray a -> HashMap Int (Spray a, Term a) -> Spray a
sprayDivisionRemainder' p qsltqs = ogo p zeroSpray
  where
    n = HM.size qsltqs
    g :: Term a -> Spray a -> Spray a -> (Spray a, Spray a)
    g lts s r = (HM.delete (fst lts) s, addTerm r lts)
    go :: Term a -> Spray a -> Spray a -> Int -> Bool -> (Spray a, Spray a)
    go lts !s r !i !divoccured
      | divoccured = (s, r)
      | i == n     = g lts s r 
      | otherwise  = go lts news r (i+1) newdivoccured
        where
          (q, ltq)      = qsltqs HM.! i
          newdivoccured = divides ltq lts
          news = if newdivoccured
            then s ^-^ multSprayByTerm q (quotient lts ltq)
            else s
    ogo :: Spray a -> Spray a -> Spray a
    ogo !s !r 
      | isZeroSpray s    = r
      | otherwise        = ogo s' r'
        where
          (s', r') = go (leadingTerm s) s r 0 False

-- combinations of two among n
combn2 :: Int -> Int -> HashMap Int (Int, Int)
combn2 n s = HM.fromList (zip range0 (zip row1 row2)) 
  where
    range0 = [0 .. n-2]
    range1 = [1 .. n-1]
    row1   = drop s $ concatMap (\i -> [0 .. i-1]) range1 
    row2   = drop s $ concatMap (\i -> replicate i i) range1

-- the "S polynomial"
sPolynomial :: (Eq a, AlgField.C a) 
               => (Spray a, Term a) -> (Spray a, Term a) -> Spray a
sPolynomial (p, (lpowsP, lcoefP)) (q, (lpowsQ, lcoefQ)) = 
  multSprayByTerm p wp ^-^ multSprayByTerm q wq
  where
    (lexpntsP, lexpntsQ) = harmonize (lpowsP, lpowsQ)
    gamma = S.zipWith max lexpntsP lexpntsQ
    betaP = S.zipWith (-) gamma lexpntsP
    betaQ = S.zipWith (-) gamma lexpntsQ
    wp = (makePowers betaP, AlgField.recip lcoefP)
    wq = (makePowers betaQ, AlgField.recip lcoefQ)

-- | groebner basis, not minimal and not reduced
groebner00 :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
groebner00 sprays = go 0 j0 combins0 spraysMap
  where
    j0       = length sprays
    combins0 = combn2 j0 0
    ltsprays       = map leadingTerm sprays
    spraysltsprays = zip sprays ltsprays 
    spraysMap      = HM.fromList (zip [0 .. j0-1] spraysltsprays)
    go :: Int -> Int -> HashMap Int (Int, Int) 
          -> HashMap Int (Spray a, Term a) -> [Spray a]
    go !i !j !combins !gpolysMap
      | i == length combins = map fst (HM.elems gpolysMap)
      | otherwise           = go i' j' combins' gpolysMap'
        where
          (k, l)   = combins HM.! i
          sfg      = sPolynomial (gpolysMap HM.! k) (gpolysMap HM.! l)
          sbarfg   = sprayDivisionRemainder' sfg gpolysMap
          ltsbarfg = leadingTerm sbarfg
          (i', j', gpolysMap', combins') = if isZeroSpray sbarfg
            then
              (i+1, j, gpolysMap, combins)
            else
              ( 0
              , j+1
              , HM.insert j (sbarfg, ltsbarfg) gpolysMap
              , combn2 (j+1) (i+1)
              )

-- | groebner basis, minimal but not reduced
groebner0 :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
groebner0 sprays = 
  if n <= 1 then sprays else [basis00 !! k | k <- [0 .. n-1] \\ discard]
  where
    n       = length basis00
    basis00 = groebner00 sprays
    go :: Int -> [Int] -> [Int]
    go !i toRemove
      | i == n    = toRemove
      | otherwise = go (i+1) toRemove'
        where
          ltf    = leadingTerm (basis00 !! i)
          toDrop = toRemove ++ [i]
          igo :: Int -> Bool
          igo !j 
            | j == n          = False
            | j `elem` toDrop = igo (j+1)
            | otherwise       = ok || igo (j+1)
              where 
                ok = divides (leadingTerm (basis00 !! j)) ltf
          toRemove' = if igo 0 then toDrop else toRemove
    discard = go 0 []

-- | Reduces a Gröbner basis
reduceGroebnerBasis :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
reduceGroebnerBasis gbasis = 
  if length gbasis >= 2 
    then map reduction [0 .. n-1] 
    else ngbasis
  where
    normalize :: Spray a -> Spray a
    normalize spray = spray /> coef
      where
        (_, coef) = leadingTerm spray
    ngbasis = map normalize gbasis
    n       = length ngbasis
    reduction :: Int -> Spray a
    reduction i = sprayDivisionRemainder (ngbasis !! i) rest
      where
        rest = [ngbasis !! k | k <- [0 .. i-1] ++ [i+1 .. n-1]]

-- | Gröbner basis, always minimal and possibly reduced
--
-- prop> groebner sprays True == reduceGroebnerBasis (groebner sprays False)
groebner ::
     forall a. (Eq a, AlgField.C a) 
  => [Spray a] -- ^ list of sprays 
  -> Bool      -- ^ whether to return the reduced basis
  -> [Spray a]
groebner sprays reduced = 
  if reduced then reduceGroebnerBasis gbasis0 else map normalize gbasis0
  where
    gbasis0 = groebner0 sprays
    normalize :: Spray a -> Spray a
    normalize spray = spray /> coef
      where
        (_, coef) = leadingTerm spray


-- elementary symmetric polynomials -------------------------------------------

-- | combinations of k elements among a list
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf _ []        = error "combinationsOf: should not happen."
combinationsOf 1 as        = map pure as
combinationsOf k as@(_:xs) = 
  run (l-1) (k-1) as $ combinationsOf (k-1) xs
  where
    l = length as
    run :: Int -> Int -> [a] -> [[a]] -> [[a]]
    run n i ys cs 
      | n == i    = map (ys ++) cs
      | otherwise = map (q:) cs ++ run (n-1) i qs (drop dc cs)
      where
        f :: [a] -> (a, [a])
        f []     = error "combinationsOf: should not happen."
        f (b:bs) = (b, bs)
        (q, qs)  = f (take (n-i+1) ys)
        dc       = product [(n-k+1) .. (n-1)] `div` product [1 .. i-1]

-- | generates all permutations of a binary sequence
permutationsBinarySequence :: Int -> Int -> [Seq Int]
permutationsBinarySequence nzeros nones = 
  let n = nzeros + nones in 
    map (binarySequence n) (combinationsOf nones [0 .. n-1])
  where
    binarySequence :: Int -> [Int] -> Seq Int
    binarySequence n combo = fromFunction n f 
      where
        f :: Int -> Int
        f i = fromEnum (i `elem` combo)

-- | Elementary symmetric polynomial
--
-- >>> putStrLn $ prettySpray' (esPolynomial 3 2)
-- (1)*x1.x2 + (1)*x1.x3 + (1)*x2.x3
esPolynomial ::
     (AlgRing.C a, Eq a) 
  => Int -- ^ number of variables
  -> Int -- ^ index
  -> Spray a
esPolynomial n k
  | k < 0 || n < 0 
    = error "esPolynomial: both arguments must be positive integers."
  | k > n     = zeroSpray
  | k == 0    = unitSpray
  | otherwise = spray
  where
    perms = permutationsBinarySequence (n-k) k
    spray = HM.fromList $ map (\expts -> (makePowers expts, AlgRing.one)) perms

-- | Power sum polynomial
psPolynomial ::
     forall a. (AlgRing.C a, Eq a) 
  => Int -- ^ number of variables
  -> Int -- ^ power
  -> Spray a
psPolynomial n k
  | k < 0 || n < 0 
    = error "psPolynomial: both arguments must be positive integers."
  | k > n     = AlgAdd.zero
  | k == 0    = n .^ unitSpray
  | otherwise = spray
  where
    spray = HM.fromList $ map f [1 .. n]
    f :: Int -> (Powers, a)
    f j = (Powers expts j, AlgRing.one)
      where
        expts = S.replicate (j-1) 0 |> k

-- | Whether a spray is a symmetric polynomial, an inefficient algorithm 
-- (use the function with the same name in the __jackpolynomials__ package 
-- if you need efficiency)
isSymmetricSpray :: forall a. (AlgField.C a, Eq a) => Spray a -> Bool
isSymmetricSpray spray = check1 && check2 
  where
    n = numberOfVariables spray
    indices = [1 .. n]
    gPolys  = map (\i -> esPolynomial n i ^-^ lone (n + i)) indices
    gbasis  = groebner0 gPolys
    spray'  = removeConstantTerm spray
    g       = sprayDivisionRemainder spray' gbasis
    gpowers = HM.keys g
    check1  = minimum (map nvariables gpowers) > n
    expnts  = map exponents gpowers
    check2  = DF.all (DF.all (0 ==)) (map (S.take n) expnts) 

-- | Whether a spray can be written as a polynomial of a given list of sprays;
-- this polynomial is returned if this is true
--
-- >>> x = lone 1 :: Spray Rational
-- >>> y = lone 2 :: Spray Rational
-- >>> p1 = x ^+^ y
-- >>> p2 = x ^-^ y
-- >>> p = p1 ^*^ p2
-- 
-- prop> isPolynomialOf p [p1, p2] == (True, Just $ x ^*^ y)
isPolynomialOf :: forall a. (AlgField.C a, Eq a) 
                  => Spray a -> [Spray a] -> (Bool, Maybe (Spray a))
isPolynomialOf spray sprays = 
  if isConstant spray 
    then (True, Just spray) 
    else result 
  where
    nov = numberOfVariables spray
    n   = maximum $ map numberOfVariables sprays
    result
      | nov > n   = (False, Nothing)
      | otherwise = (checks, poly)
        where
          m            = length sprays
          yPolys       = map (\i -> lone (n + i) :: Spray a) [1 .. m]
          gPolys       = zipWith (^-^) sprays yPolys
          gbasis0      = groebner0 gPolys
          constantTerm = getConstantTerm spray
          spray'       = removeConstantTerm spray
          g            = sprayDivisionRemainder spray' gbasis0
          gpowers      = HM.keys g
          check1       = minimum (map nvariables gpowers) > n
          check2       = DF.all (DF.all (0 ==)) (map (S.take n . exponents) gpowers)
          checks       = check1 && check2
          poly         = if checks
            then Just g''
            else Nothing
          g' = dropXis g
          g'' = if constantTerm == AlgAdd.zero 
            then g' 
            else addTerm g' (Powers S.empty 0, constantTerm)
          dropXis = HM.mapKeys f
          f (Powers expnnts nv) = Powers (S.drop n expnnts) (nv - n)


-- resultant ------------------------------------------------------------------

-- | sylvester matrix
sylvesterMatrix :: AlgAdd.C a => [a] -> [a] -> Matrix a
sylvesterMatrix x y = fromLists (xrows ++ yrows) 
  where
    m = length x - 1
    n = length y - 1
    xrows = [replicate i AlgAdd.zero ++ x ++ replicate (n-i-1) AlgAdd.zero 
             | i <- [0 .. n-1]]
    yrows = [replicate i AlgAdd.zero ++ y ++ replicate (m-i-1) AlgAdd.zero 
             | i <- [0 .. m-1]]

-- | "truncated" Sylvester matrix
sylvesterMatrix' :: AlgRing.C a => [a] -> [a] -> Int -> Matrix a
sylvesterMatrix' x y k = if s == 0 
  then fromLists [[AlgRing.one]] -- plays the role of the empty matrix: 
                                 -- the point to get is determinant=1 
                                 -- (because the empty matrix is not allowed
                                 -- in the matrix package)
  else submatrix 1 s 1 s $ fromLists (xrows ++ yrows) 
  where
    m = length x - 1
    n = length y - 1
    s = m + n - 2*k
    xrows = [replicate i AlgAdd.zero ++ x ++ replicate (n-i-1) AlgAdd.zero 
             | i <- [0 .. n-1-k]]
    yrows = [replicate i AlgAdd.zero ++ y ++ replicate (m-i-1) AlgAdd.zero 
             | i <- [0 .. m-1-k]]

-- | the coefficients of a spray as a univariate spray in x_1 with 
-- spray coefficients
sprayCoefficients :: (Eq a, AlgRing.C a) => Spray a -> [Spray a]
sprayCoefficients spray = 
  if n == 0 
    then [constantTerm]
    else sprays
  where
    n = numberOfVariables spray 
    spray' = removeConstantTerm spray
    (powers', coeffs') = unzip (HM.toList spray')
    expnts' = map exponents powers'
    constantTerm = (constantSpray . getConstantTerm) spray
    xpows              = map (`index` 0) expnts'
    powers''           = map ((\s -> Powers s (S.length s)) . S.deleteAt 0) expnts'
    sprays''           = zipWith (curry fromTerm) powers'' coeffs'
    imap               = IM.fromListWith (^+^) (zip xpows sprays'')
    imap'              = IM.insertWith (^+^) 0 constantTerm imap
    permutation = [2 .. n] ++ [1]
    deg = maximum xpows
    sprays = [
        permuteVariables permutation (fromMaybe AlgAdd.zero (IM.lookup i imap')) 
        | i <- [deg, deg-1 .. 0]
      ]

-- | Resultant of two /univariate/ sprays
resultant1 :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> a
resultant1 p q = 
  if n <= 1 
    then detLaplace $ sylvesterMatrix pcoeffs qcoeffs
    else error "resultant1: the two sprays must be univariate."
  where
    n = max (numberOfVariables p) (numberOfVariables q)
    pexpnts = 
      map ((`index` 0) . exponents) $ HM.keys $ removeConstantTerm p
    qexpnts = 
      map ((`index` 0) . exponents) $ HM.keys $ removeConstantTerm q
    p0 = getConstantTerm p
    q0 = getConstantTerm q
    pcoeffs = if null pexpnts 
      then [p0]
      else [getCoefficient' (Powers (S.singleton i) 1) p 
            | i <- [maxp, maxp-1 .. 1]] ++ [p0]
      where
        maxp = maximum pexpnts
    qcoeffs = if null qexpnts 
      then [q0]
      else [getCoefficient' (Powers (S.singleton i) 1) q 
            | i <- [maxq, maxq-1 .. 1]] ++ [q0]
      where
        maxq = maximum qexpnts

-- | Subresultants of two /univariate/ sprays
subresultants1 :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> [a]
subresultants1 p q = if n <= 1 
  then map (detLaplace . sylvesterMatrix' pcoeffs qcoeffs) [0 .. min d e - 1]
  else error "subresultants1: the two sprays must be univariate."
  where
    n = max (numberOfVariables p) (numberOfVariables q)
    pexpnts = 
      map ((`index` 0) . exponents) $ HM.keys $ removeConstantTerm p
    qexpnts = 
      map ((`index` 0) . exponents) $ HM.keys $ removeConstantTerm q
    p0 = getConstantTerm p
    q0 = getConstantTerm q
    pcoeffs = if null pexpnts 
      then [p0]
      else [getCoefficient' (Powers (S.singleton i) 1) p 
            | i <- [maxp, maxp-1 .. 1]] ++ [p0]
      where
        maxp = maximum pexpnts
    qcoeffs = if null qexpnts 
      then [q0]
      else [getCoefficient' (Powers (S.singleton i) 1) q 
            | i <- [maxq, maxq-1 .. 1]] ++ [q0]
      where
        maxq = maximum qexpnts
    d = length pcoeffs
    e = length qcoeffs

-- | Resultant of two sprays
resultant :: (Eq a, AlgRing.C a) 
  => Int     -- ^ indicator of the variable with respect to which the resultant is desired (e.g. 1 for x)
  -> Spray a 
  -> Spray a 
  -> Spray a
resultant var p q = 
  if var >= 1 && var <= n 
    then permuteVariables permutation' det
    else error "resultant: invalid variable index."
  where
    n = max (numberOfVariables p) (numberOfVariables q)
    permutation  = [n-var+2 .. n] ++ [1 .. n-var+1]
    permutation' = [var .. n] ++ [1 .. var-1]
    p' = permuteVariables permutation p
    q' = permuteVariables permutation q
    det = detLaplace $ 
          sylvesterMatrix (sprayCoefficients p') (sprayCoefficients q')

-- | Subresultants of two sprays
subresultants :: (Eq a, AlgRing.C a) 
  => Int     -- ^ indicator of the variable with respect to which the subresultants are desired (e.g. 1 for x)
  -> Spray a 
  -> Spray a 
  -> [Spray a]
subresultants var p q 
  | var < 1 = error "subresultants: invalid variable index."
  | var > n = error "subresultants: too large variable index."
  | otherwise = map (permute' . detLaplace . sylvesterMatrix' pcoeffs qcoeffs) 
                    [0 .. min d e - 1]
  where
    pcoeffs = sprayCoefficients p'
    qcoeffs = sprayCoefficients q'
    d = length pcoeffs
    e = length qcoeffs
    n = max (numberOfVariables p) (numberOfVariables q)
    permutation = var : [1 .. var-1] ++ [var+1 .. n]
    permute     = permuteVariables permutation
    p' = permute p 
    q' = permute q 
    permutation' = [2 .. var] ++ (1 : [var+1 .. n])
    permute'     = permuteVariables permutation'

-- | Resultant of two sprays with coefficients in a field; this function is more 
-- efficient than the function `resultant`
resultant' :: forall a. (Eq a, AlgField.C a) 
  => Int     -- ^ indicator of the variable with respect to which the resultant is desired (e.g. 1 for x)
  -> Spray a 
  -> Spray a 
  -> Spray a
resultant' var sprayA sprayB 
  | var < 1 || var > n                         
    = error "resultant': invalid variable index." 
  | isZeroSpray sprayA || isZeroSpray sprayB 
    = zeroSpray
  | otherwise 
    = permuteVariables permutation' $ go unitSpray unitSpray s0 p0 q0
  where
    n = max (numberOfVariables sprayA) (numberOfVariables sprayB)
    permutation  = [n-var+1 .. n] ++ [1 .. n-var]
    permutation' = [var+1 .. n] ++ [1 .. var] 
    sprayA' = permuteVariables permutation sprayA
    sprayB' = permuteVariables permutation sprayB
    degA = degree n sprayA'
    degB = degree n sprayB'
    content :: Spray a -> Spray a
    content spray = foldl1' gcdSpray (sprayCoefficients' n spray)
    exactDivisionBy :: Spray a -> Spray a -> Spray a
    exactDivisionBy b a = 
      if isZeroSpray remainder 
        then quo 
        else error "exactDivisionBy: should not happen."
      where
        (quo, remainder) = sprayDivision a b
    contA = content sprayA'
    contB = content sprayB'
    sprayA'' = exactDivisionBy contA sprayA'
    sprayB'' = exactDivisionBy contB sprayB'
    t = contA^**^degB ^*^ contB^**^degA
    s0 = if degA < degB && odd degA && odd degB 
      then AlgAdd.negate unitSpray :: Spray a
      else unitSpray
    (p0, q0) = if degA >= degB
      then (sprayA'', sprayB'')
      else (sprayB'', sprayA'')
    go :: Spray a -> Spray a -> Spray a -> Spray a -> Spray a -> Spray a
    go g h s p q = 
      if degq' == 0
        then s' ^*^ t ^*^ h''
        else go g' h' s' p' q'
        where
          degp           = degree n p
          degq           = degree n q
          delta          = degp - degq
          s' = if odd degp && odd degq 
            then AlgAdd.negate s 
            else s
          (_, (_, r)) = pseudoDivision n p q
          p'             = q
          q'             = exactDivisionBy (g ^*^ h^**^delta) r
          (degp', ellp') = degreeAndLeadingCoefficient n p'
          (degq', ellq') = degreeAndLeadingCoefficient n q'
          g'  = ellp'
          h'  = exactDivisionBy (h^**^delta) (h ^*^ g'^**^delta)
          h'' = exactDivisionBy (h'^**^degp') (h' ^*^ ellq'^**^degp')


-- GCD stuff ------------------------------------------------------------------

-- | the coefficients of a spray as a univariate spray in x_n with 
-- spray coefficients
sprayCoefficients' :: (Eq a, AlgRing.C a) => Int -> Spray a -> [Spray a]
sprayCoefficients' n spray 
  | numberOfVariables spray /= n = [spray]
  | n == 0                       = [constantSpray constantTerm]
  | otherwise                    = sprays 
  where
    permutation = [2 .. n] ++ [1]
    spray'      = permuteVariables permutation spray
    spray'' = removeConstantTerm spray'
    (powers', coeffs') = unzip (HM.toList spray'')
    expnts' = map exponents powers'
    constantTerm = getConstantTerm spray'
    xpows = map (`index` 0) expnts'
    powers'' = map ((\s -> Powers s (S.length s)) . S.deleteAt 0) expnts'
    sprays'' = zipWith (curry fromTerm) powers'' coeffs'
    imap   = IM.fromListWith (^+^) (zip xpows sprays'')
    imap'  = IM.insertWith (^+^) 0 (constantSpray constantTerm) imap
    deg    = maximum xpows
    sprays = [
        fromMaybe AlgAdd.zero (IM.lookup i imap')
        | i <- [deg, deg-1 .. 0]
      ]

-- | the degree of a spray as a univariate spray in x_n with spray coefficients
degree :: (Eq a, AlgRing.C a) => Int -> Spray a -> Int
degree n spray 
  | isConstant spray = 
      if isZeroSpray spray 
        then minBound -- (should not happen)
        else 0
  | numberOfVariables spray /= n = 0
  | otherwise                    = maximum xpows
    where
      permutation = [2 .. n] ++ [1]
      spray'      = permuteVariables permutation spray
      powers'     = HM.keys $ removeConstantTerm spray'
      xpows       = map ((`index` 0) . exponents) powers'

-- | the degree and the leading coefficient of a spray as a univariate spray 
-- in x_n with spray coefficients
degreeAndLeadingCoefficient :: (Eq a, AlgRing.C a) 
                                => Int -> Spray a -> (Int, Spray a)
degreeAndLeadingCoefficient n spray 
  | n == 0                       = (
                                    if constantTerm == AlgAdd.zero 
                                      then minBound -- (should not happen)
                                      else 0, 
                                    constantSpray constantTerm
                                   )
  | numberOfVariables spray /= n = (0, spray)
  | otherwise                    = (deg, leadingCoeff)
  where
    permutation  = [2 .. n] ++ [1]
    spray'       = permuteVariables permutation spray
    constantTerm = getConstantTerm spray'
    spray''            = removeConstantTerm spray'
    (powers', coeffs') = unzip (HM.toList spray'')
    expnts'            = map exponents powers'
    xpows = map (`index` 0) expnts'
    deg   = maximum xpows
    is    = elemIndices deg xpows
    powers'' = map 
      ((\s -> Powers s (S.length s)) . (\i -> S.deleteAt 0 (expnts' !! i))) is
    coeffs'' = [coeffs' !! i | i <- is]
    leadingCoeff = sumTerms (zip powers'' coeffs'')

-- | Pseudo-division of two sprays, assuming degA >= degB >= 0
pseudoDivision :: (Eq a, AlgRing.C a)
  => Int                           -- ^ number of variables
  -> Spray a                       -- ^ A
  -> Spray a                       -- ^ B
  -> (Spray a, (Spray a, Spray a)) -- ^ (c, (Q, R)) such that c^*^A = B^*^Q ^+^ R
pseudoDivision n sprayA sprayB 
  | degB == minBound = error "pseudoDivision: pseudo-division by 0."
  | degA < degB      = error "pseudoDivision: degree(A) < degree(B)."
  | otherwise        = (ellB ^**^ delta , go sprayA zeroSpray delta)
  where
    degA         = degree n sprayA
    (degB, ellB) = degreeAndLeadingCoefficient n sprayB
    delta        = degA - degB + 1
    go sprayR sprayQ e = 
      if isZeroSpray sprayR || degR < degB
        then (q ^*^ sprayQ, q ^*^ sprayR)
        else go (ellB ^*^ sprayR ^-^ sprayS ^*^ sprayB) 
                (ellB ^*^ sprayQ ^+^ sprayS) 
                (e - 1)
      where
        (degR, ellR) = degreeAndLeadingCoefficient n sprayR
        q            = ellB ^**^ e
        sprayS       = multSprayByTerm ellR (loneTerm' n (degR - degB))

-- | recursive GCD function
gcdKX1dotsXn :: forall a. (Eq a, AlgField.C a) 
                => Int -> Spray a -> Spray a -> Spray a
gcdKX1dotsXn n sprayA sprayB
  | n == 0              = constantSpray $ gcdKX0 sprayA sprayB
  | degB > degA         = gcdKX1dotsXn n sprayB sprayA 
  | isZeroSpray sprayB  = sprayA
  | otherwise           = go sprayA' sprayB' unitSpray unitSpray
  where
    gcdKX0 :: Spray a -> Spray a -> a
    gcdKX0 = const $ const AlgRing.one 
    n' = max (numberOfVariables sprayA) (numberOfVariables sprayB)
    degA = degree n' sprayA
    degB = degree n' sprayB
    gcdKX1dotsXm = gcdKX1dotsXn (n-1)
    content :: Spray a -> Spray a
    content spray = foldl1' gcdKX1dotsXm (sprayCoefficients' n' spray)
    exactDivisionBy :: Spray a -> Spray a -> Spray a
    exactDivisionBy b a = 
      if isZeroSpray remainder 
        then quo 
        else error "exactDivisionBy: should not happen."
      where
        (quo, remainder) = sprayDivision a b
    reduceSpray :: Spray a -> Spray a
    reduceSpray spray = exactDivisionBy (content spray) spray 
    contA   = content sprayA
    contB   = content sprayB
    d       = gcdKX1dotsXm contA contB 
    sprayA' = exactDivisionBy contA sprayA 
    sprayB' = exactDivisionBy contB sprayB 
    go :: Spray a -> Spray a -> Spray a -> Spray a -> Spray a
    go sprayA'' sprayB'' g h 
      | isZeroSpray sprayR            = d ^*^ reduceSpray sprayB''
      | numberOfVariables sprayR == 0 = d
      | otherwise = go sprayB'' 
                       (exactDivisionBy (g ^*^ h^**^delta) sprayR)
                       ellA''
                       (exactDivisionBy (h^**^delta) (h ^*^ g^**^delta))
        where
          (_, (_, sprayR)) = pseudoDivision n' sprayA'' sprayB''
          (degA'', ellA'') = degreeAndLeadingCoefficient n' sprayA''
          degB''           = degree n' sprayB'' 
          delta            = degA'' - degB''

-- | Greatest common divisor of two sprays with coefficients in a field
gcdSpray :: forall a. (Eq a, AlgField.C a) => Spray a -> Spray a -> Spray a
gcdSpray sprayA sprayB = gcdKX1dotsXn n sprayA sprayB 
  where
    n = max (numberOfVariables sprayA) (numberOfVariables sprayB)


-- Matrices -------------------------------------------------------------------

-- | Determinant of a matrix with entries in a ring by using Laplace 
-- expansion (this is slow); the __numeric-prelude__ package provides some 
-- stuff to deal with matrices over a ring but it does not provide the 
-- determinant
detLaplace :: forall a. (Eq a, AlgRing.C a) => Matrix a -> a
detLaplace b = 
  if nrows b == ncols b 
    then detUnsafe b
    else error "detLaplace: the matrix is not square."
  where 
    detUnsafe m = if nrows m == 1 
      then 
        m DM.! (1,1)
      else 
        suml1 
          [negateIf i (times (m DM.! (i,1)) (detUnsafe (minorMatrix i 1 m))) 
          | i <- [1 .. nrows m]]
    suml1      = foldl1' (AlgAdd.+)
    negateIf i = if even i then AlgAdd.negate else id
    times :: a -> a -> a
    times x y = if x == AlgAdd.zero then AlgAdd.zero else x AlgRing.* y

-- | Determinant of a matrix over a ring by using Laplace expansion; this is 
-- the same as `detLaplace` but for a matrix from the __numeric-prelude__ 
-- package
detLaplace' :: forall a. (Eq a, AlgRing.C a) => MathMatrix.T a -> a
detLaplace' m = detLaplace (DM.fromLists $ MathMatrix.rows m) 

-- | Characteristic polynomial of a square matrix
--
-- >>> import Data.Matrix (Matrix, fromLists)
-- >>> m = fromLists [ [12, 16, 4]
-- >>>               , [16, 2, 8]
-- >>>               , [8, 18, 10] ] :: Matrix Int
-- >>> spray = characteristicPolynomial m
-- >>> putStrLn $ prettyNumSpray spray
-- -x^3 + 24*x^2 + 268*x - 1936
characteristicPolynomial :: (Eq a, AlgRing.C a) => Matrix a -> Spray a
characteristicPolynomial m = 
  if nrows m /= ncols m 
    then error "characteristicPolynomial: the matrix is not square."
    else detLaplace m'
  where
    m' = DM.mapPos f m
    f (i, j) mij = if i == j 
      then constantSpray mij ^-^ x
      else constantSpray mij
    x = lone 1


-- Ratios of sprays -----------------------------------------------------------

-- | A @RatioOfSprays a@ object represents a fraction of two multivariate 
-- polynomials whose coefficients are of type @a@, which represents a field. 
-- These two polynomials are represented by two @Spray a@ objects. Generally 
-- we do not use this constructor to build a ratio of sprays: we use the `%//%`
-- operator instead, because it always returns an irreducible ratio of sprays, 
-- meaning that its corresponding fraction of polynomials is irreducible, i.e. 
-- its numerator and its denominator are coprime. You can use this constructor 
-- if you are /sure/ that the numerator and the denominator are coprime. This 
-- can save some computation time, but unfortunate consequences can occur if 
-- the numerator and the denominator are not coprime. An arithmetic operation
-- on ratios of sprays always returns an irreducible ratio of sprays under the 
-- condition that the ratios of sprays it involves are irreducible. Moreover, 
-- it never returns a ratio of sprays with a constant denominator other than 
-- the unit spray. If you use this constructor with a constant denominator, 
-- always set this denominator to the unit spray (by dividing the numerator 
-- by the constant value of the denominator).
data RatioOfSprays a = RatioOfSprays
  { _numerator   :: Spray a
  , _denominator :: Spray a
  }
  deriving Show

type RatioOfQSprays = RatioOfSprays Rational

instance (Eq a, AlgField.C a) => HasVariables (RatioOfSprays a) where
  type BaseRing (RatioOfSprays a) = a
  --
  type VariablesType (RatioOfSprays a) = Spray a
  --
  substitute :: [Maybe a] -> RatioOfSprays a -> RatioOfSprays a
  substitute subs (RatioOfSprays p q) = 
    substitute subs p %//% substitute subs q  
  --
  evaluate :: RatioOfSprays a -> [a] -> a
  evaluate (RatioOfSprays p q) xyz = evaluate p xyz AlgField./ evaluate q xyz
  --
  changeVariables :: RatioOfSprays a -> [Spray a] -> RatioOfSprays a
  changeVariables rOS newVariables = 
    if length newVariables < numberOfVariables rOS
      then 
        error "changeVariables: not enough new variables provided."
      else
        changeVariables (_numerator rOS) newVariables 
          %//% changeVariables (_denominator rOS) newVariables 
  --
  numberOfVariables :: RatioOfSprays a -> Int
  numberOfVariables (RatioOfSprays p q) = 
    max (numberOfVariables p) (numberOfVariables q)
  --
  permuteVariables :: [Int] -> RatioOfSprays a -> RatioOfSprays a
  permuteVariables permutation (RatioOfSprays p q) = 
    permuteVariables permutation p %//% permuteVariables permutation q
  --
  swapVariables :: (Int, Int) -> RatioOfSprays a -> RatioOfSprays a
  swapVariables (i, j) (RatioOfSprays p q) = 
    swapVariables (i, j) p %//% swapVariables (i, j) q
  --
  derivative :: Int -> RatioOfSprays a -> RatioOfSprays a
  derivative i (RatioOfSprays p q) = (p' ^*^ q ^-^ p ^*^ q') %//% (q ^*^ q)
    where
      p' = derivative i p
      q' = derivative i q

-- | quotients of two univariate sprays by their gcd
-- we use `sprayDivision0` because this function is called 
-- (by `irreducibleFraction`) with non-constant sprays only
quotientsByGCD :: 
  (Eq a, AlgField.C a) => Spray a -> Spray a -> (Spray a, Spray a)
quotientsByGCD sprayA sprayB = 
  if isUnivariate sprayA && isUnivariate sprayB
    then
      go sprayA sprayB unitSpray zeroSpray zeroSpray unitSpray
    else
      (exactDivision sprayA g, exactDivision sprayB g)
    where 
      exactDivision p q = fst (sprayDivision0 p q)
      g = gcdSpray sprayA sprayB
      go oldr r olds s oldt t 
        | isZeroSpray r = (c *^ AlgAdd.negate t, c *^ s) -- monic denominator
        | otherwise     = 
            go r remainder s (olds ^-^ quo ^*^ s) t (oldt ^-^ quo ^*^ t)
          where
            (quo, remainder) = univariateSprayDivision oldr r
            c = AlgField.recip (snd $ leadingTerm s)

-- | irreducible fraction of sprays
irreducibleFraction ::
  (Eq a, AlgField.C a) => Spray a -> Spray a -> RatioOfSprays a
irreducibleFraction p q = adjustFraction rOS
  where
    rOS = if isConstant p || isConstant q
      then RatioOfSprays p q 
      else let (a, b) = quotientsByGCD p q in RatioOfSprays a b

-- | set denominator to 1 if it is constant
adjustFraction :: (Eq a, AlgField.C a) => RatioOfSprays a -> RatioOfSprays a
adjustFraction (RatioOfSprays p q) = if isConstant q 
  then RatioOfSprays (p /^ getConstantTerm q) unitSpray
  else RatioOfSprays p q

instance (AlgRing.C a, Eq a) => Eq (RatioOfSprays a) where
  (==) :: RatioOfSprays a -> RatioOfSprays a -> Bool
  (==) (RatioOfSprays p q) (RatioOfSprays p' q') = 
    isZeroSpray (p ^*^ q'  ^-^  p' ^*^ q)

instance (AlgField.C a, Eq a) => AlgAdd.C (RatioOfSprays a) where
  (+) :: RatioOfSprays a -> RatioOfSprays a -> RatioOfSprays a
  (+) (RatioOfSprays p q) (RatioOfSprays p' q') = 
    irreducibleFraction (p ^*^ q'  ^+^  p' ^*^ q) (q ^*^ q')
  zero :: RatioOfSprays a
  zero = RatioOfSprays zeroSpray unitSpray
  negate :: RatioOfSprays a -> RatioOfSprays a
  negate (RatioOfSprays p q) = RatioOfSprays (negateSpray p) q

instance (AlgField.C a, Eq a) => AlgMod.C a (RatioOfSprays a) where
  (*>) :: a -> RatioOfSprays a -> RatioOfSprays a
  lambda *> (RatioOfSprays p q) = RatioOfSprays (lambda *^ p) q

instance (AlgField.C a, Eq a) => AlgRightMod.C a (RatioOfSprays a) where
  (<*) :: RatioOfSprays a -> a -> RatioOfSprays a
  rOS <* lambda = lambda AlgMod.*> rOS

instance (AlgField.C a, Eq a) => AlgMod.C (Spray a) (RatioOfSprays a) where
  (*>) :: Spray a -> RatioOfSprays a -> RatioOfSprays a
  spray *> (RatioOfSprays p q) = irreducibleFraction (spray ^*^ p) q

instance (AlgField.C a, Eq a) => AlgRightMod.C (Spray a) (RatioOfSprays a) where
  (<*) :: RatioOfSprays a -> Spray a -> RatioOfSprays a
  rOS <* spray = spray AlgMod.*> rOS

instance (AlgField.C a, Eq a) => AlgRing.C (RatioOfSprays a) where
  (*) :: RatioOfSprays a -> RatioOfSprays a -> RatioOfSprays a
  (*) (RatioOfSprays p q) (RatioOfSprays p' q') = 
    irreducibleFraction (p ^*^ p') (q ^*^ q')
  (^) :: RatioOfSprays a -> Integer -> RatioOfSprays a
  (^) (RatioOfSprays p q) n = if n >= 0 
    then RatioOfSprays (p AlgRing.^ n) (q AlgRing.^ n)
    else RatioOfSprays (q AlgRing.^ (-n)) (p AlgRing.^ (-n))
  one :: RatioOfSprays a
  one = RatioOfSprays unitSpray unitSpray

instance (AlgField.C a, Eq a) => AlgField.C (RatioOfSprays a) where
  recip :: RatioOfSprays a -> RatioOfSprays a
  recip (RatioOfSprays p q) = RatioOfSprays q p

infixl 7 %:%
-- | Ratio of sprays from numerator and denominator, 
-- __without reducing the fraction__
(%:%) :: Spray a -> Spray a -> RatioOfSprays a 
(%:%) = RatioOfSprays

infixl 7 %//%
-- | Irreducible ratio of sprays from numerator and denominator; alias of @(^/^)@
(%//%) :: (Eq a, AlgField.C a) => Spray a -> Spray a -> RatioOfSprays a 
(%//%) = irreducibleFraction 

infixl 7 ^/^
-- | Irreducible ratio of sprays from numerator and denominator; alias of @(%//%)@
(^/^) :: (Eq a, AlgField.C a) => Spray a -> Spray a -> RatioOfSprays a 
(^/^) = irreducibleFraction 

infixl 7 %/%
-- | Division of a ratio of sprays by a spray; the result is an 
-- irreducible fraction
(%/%) :: (Eq a, AlgField.C a) => RatioOfSprays a -> Spray a -> RatioOfSprays a 
(%/%) rOS spray = rOS AlgRing.* RatioOfSprays unitSpray spray 

-- | Whether a ratio of sprays is constant; same as `isConstant`
isConstantRatioOfSprays :: (Eq a, AlgField.C a) => RatioOfSprays a -> Bool
isConstantRatioOfSprays = isConstant

-- | Whether a ratio of sprays actually is polynomial, that is, whether its 
-- denominator is a constant spray (and then it should be the unit spray)
--
-- >>> x = qlone 1
-- >>> y = qlone 2
-- >>> p = x^**^4 ^-^ y^**^4
-- >>> q = x ^-^ y
-- >>> isPolynomialRatioOfSprays $ p %//% q
-- True
-- >>> isPolynomialRatioOfSprays $ p %:% q
-- False
isPolynomialRatioOfSprays :: (Eq a, AlgRing.C a) => RatioOfSprays a -> Bool
isPolynomialRatioOfSprays = isConstant . _denominator

-- | The null ratio of sprays
zeroRatioOfSprays, zeroROS :: (AlgField.C a, Eq a) => RatioOfSprays a
zeroRatioOfSprays = AlgAdd.zero
zeroROS = AlgAdd.zero

-- | The unit ratio of sprays
unitRatioOfSprays, unitROS :: (AlgField.C a, Eq a) => RatioOfSprays a
unitRatioOfSprays = AlgRing.one
unitROS = AlgRing.one

-- | Constant ratio of sprays
constantRatioOfSprays :: (Eq a, AlgRing.C a) => a -> RatioOfSprays a
constantRatioOfSprays x = asRatioOfSprays (constantSpray x)

-- | Evaluates a ratio of sprays; same as `evaluate`
evalRatioOfSprays :: (Eq a, AlgField.C a) => RatioOfSprays a -> [a] -> a
evalRatioOfSprays = evaluate

-- | Substitutes some values to some variables of a ratio of sprays; same as `substitute`
substituteRatioOfSprays :: 
  (Eq a, AlgField.C a) => [Maybe a] -> RatioOfSprays a -> RatioOfSprays a
substituteRatioOfSprays = substitute

-- | Coerces a spray to a ratio of sprays
asRatioOfSprays :: AlgRing.C a => Spray a -> RatioOfSprays a
asRatioOfSprays spray = RatioOfSprays spray unitSpray

-- | Converts a ratio of polynomials to a ratio of sprays
fromRatioOfPolynomials :: 
  (Eq a, AlgRing.C a) => RatioOfPolynomials a -> RatioOfSprays a
fromRatioOfPolynomials rop = 
  RatioOfSprays 
    (polynomialToSpray $ NumberRatio.numerator rop) 
    (polynomialToSpray $ NumberRatio.denominator rop)  

-- | Converts a ratio of rational polynomials to a ratio of rational sprays; 
-- this is not a specialization of `fromRatioOfPolynomials` because 
-- @RatioOfQPolynomials@ is @RatioOfPolynomials a@ with 
-- @a = Rational'@, not with @a = Rational@
fromRatioOfQPolynomials :: RatioOfQPolynomials -> RatioOfQSprays
fromRatioOfQPolynomials rop = 
  RatioOfSprays 
    (qPolynomialToQSpray $ NumberRatio.numerator rop) 
    (qPolynomialToQSpray $ NumberRatio.denominator rop)  

-- | General function to print a `RatioOfSprays` object
showRatioOfSprays :: (Eq a, AlgRing.C a) 
  => ((Spray a, Spray a) -> (String, String)) -- ^ function which prints a pair of sprays that will be applied to the numerator and the denominator
  -> (String, String)                         -- ^ pair of braces to enclose the numerator and the denominator
  -> String                                   -- ^ represents the quotient bar
  -> RatioOfSprays a 
  -> String
showRatioOfSprays spraysShower braces quotientBar (RatioOfSprays p q) = 
  numeratorString ++ denominatorString
  where
    enclose = bracify braces
    (pString, qString) = spraysShower (p, q)
    numeratorString   = enclose pString
    denominatorString = if q == unitSpray
      then ""
      else quotientBar ++ enclose qString

showTwoSpraysXYZ :: (Eq a, AlgRing.C a)
  => (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String)        -- ^ used to enclose the coefficients, usually a pair of braces
  -> [String]                -- ^ typically some letters, to print the variables
  -> (Spray a, Spray a)      -- ^ the two sprays to be printed
  -> (String, String)
showTwoSpraysXYZ showCoef braces letters (spray1, spray2) =
  both (showSpray showCoef braces showMonomials) (spray1, spray2)
  where
    n = max (numberOfVariables spray1) (numberOfVariables spray2)
    showMonomials = map (unpack . showMonomialXYZ letters n)

showTwoSpraysX1X2X3 ::
     (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String)        -- ^ used to enclose the coefficients, usually a pair of braces
  -> String                  -- ^ typically a letter, to print the non-indexed variables
  -> (Spray a, Spray a)      -- ^ the two sprays to be printed
  -> (String, String)
showTwoSpraysX1X2X3 showCoef braces letter (spray1, spray2) =
  both (showSpray showCoef braces showMonomials) (spray1, spray2)
  where
    showMonomials = showMonomialsX1X2X3 letter

showTwoNumSprays :: (Num a, Ord a)
  => (a -> String)           -- ^ function mapping a positive coefficient to a string
  -> ([Seq Int] -> [String]) -- ^ prints the monomials
  -> (Spray a, Spray a)      -- ^ the two sprays to be printed
  -> (String, String)
showTwoNumSprays showPositiveCoef showMonomials =
  both (showNumSpray showMonomials showPositiveCoef)

showTwoQSprays :: 
     ([Seq Int] -> [String]) -- ^ prints the monomials
  -> (QSpray, QSpray)        -- ^ the two sprays to be printed
  -> (String, String)
showTwoQSprays = showTwoNumSprays showRatio

showTwoNumSpraysXYZ :: (AlgRing.C a, Num a, Ord a)
  => (a -> String)           -- ^ function mapping a positive coefficient to a string
  -> [String]                -- ^ typically some letters, to print the variables
  -> (Spray a, Spray a)      -- ^ the two sprays to be printed
  -> (String, String)
showTwoNumSpraysXYZ showPositiveCoef letters (spray1, spray2) =
  showTwoNumSprays showPositiveCoef showMonomials (spray1, spray2)
  where
    n = max (numberOfVariables spray1) (numberOfVariables spray2)
    showMonomials = map (unpack . showMonomialXYZ letters n)

showTwoQSpraysXYZ ::
     [String]              -- ^ typically some letters, to print the variables
  -> (QSpray, QSpray)      -- ^ the two sprays to be printed
  -> (String, String)
showTwoQSpraysXYZ = showTwoNumSpraysXYZ showRatio

showTwoNumSpraysX1X2X3 :: (Num a, Ord a)
  => (a -> String)           -- ^ function mapping a positive coefficient to a string
  -> String                  -- ^ typically a letter, to print the non-indexed variable
  -> (Spray a, Spray a)      -- ^ the two sprays to be printed
  -> (String, String)
showTwoNumSpraysX1X2X3 showPositiveCoef letter (spray1, spray2) =
  showTwoNumSprays showPositiveCoef showMonomials (spray1, spray2)
  where
    showMonomials = showMonomialsX1X2X3 letter

showTwoQSpraysX1X2X3 ::
      String               -- ^ typically a letter, to print the non-indexed variables
  -> (QSpray, QSpray)      -- ^ the two sprays to be printed
  -> (String, String)
showTwoQSpraysX1X2X3 = showTwoNumSpraysX1X2X3 showRatio

-- | Prints a ratio of sprays with numeric coefficients
showRatioOfNumSprays :: (Num a, Ord a, AlgRing.C a) 
  => (a -> String)           -- ^ function mapping a positive coefficient to a string
  -> ([Seq Int] -> [String]) -- ^ prints the monomials
  -> (String, String)        -- ^ pair of braces to enclose the numerator and the denominator
  -> String                  -- ^ represents the quotient bar
  -> RatioOfSprays a 
  -> String
showRatioOfNumSprays showPositiveCoef showMonomials = 
  showRatioOfSprays (showTwoNumSprays showPositiveCoef showMonomials)

-- | Prints a ratio of sprays with rational coefficients
showRatioOfQSprays ::  
     ([Seq Int] -> [String]) -- ^ prints the monomials
  -> (String, String)        -- ^ pair of braces to enclose the numerator and the denominator
  -> String                  -- ^ represents the quotient bar
  -> RatioOfQSprays 
  -> String
showRatioOfQSprays showMonomials = 
  showRatioOfSprays (showTwoQSprays showMonomials)

-- | Prints a ratio of sprays with numeric coefficients
showRatioOfNumSpraysXYZ :: (Num a, Ord a, AlgRing.C a) 
  => (a -> String)           -- ^ function mapping a positive coefficient to a string
  -> [String]                -- ^ typically some letters, to print the variables
  -> (String, String)        -- ^ pair of braces to enclose the numerator and the denominator
  -> String                  -- ^ represents the quotient bar
  -> RatioOfSprays a 
  -> String
showRatioOfNumSpraysXYZ showPositiveCoef letters = 
  showRatioOfSprays (showTwoNumSpraysXYZ showPositiveCoef letters)

-- | Prints a ratio of sprays with numeric coefficients
showRatioOfNumSpraysX1X2X3 :: (Num a, Ord a, AlgRing.C a) 
  => (a -> String)          -- ^ function mapping a positive coefficient to a string
  -> String                 -- ^ typically a letter, to print the variables
  -> (String, String)       -- ^ pair of braces to enclose the numerator and the denominator
  -> String                 -- ^ represents the quotient bar
  -> RatioOfSprays a 
  -> String
showRatioOfNumSpraysX1X2X3 showPositiveCoef letter = 
  showRatioOfSprays (showTwoNumSpraysX1X2X3 showPositiveCoef letter)

-- | Prints a ratio of sprays with rational coefficients
showRatioOfQSpraysXYZ ::  
     [String]                -- ^ typically some letters, to print the variables
  -> (String, String)        -- ^ pair of braces to enclose the numerator and the denominator
  -> String                  -- ^ represents the quotient bar
  -> RatioOfQSprays
  -> String
showRatioOfQSpraysXYZ letters = showRatioOfSprays (showTwoQSpraysXYZ letters)

-- | Prints a ratio of sprays with rational coefficients
showRatioOfQSpraysX1X2X3 ::  
     String                -- ^ typically a letter, to print the variables
  -> (String, String)      -- ^ pair of braces to enclose the numerator and the denominator
  -> String                -- ^ represents the quotient bar
  -> RatioOfQSprays
  -> String
showRatioOfQSpraysX1X2X3 letter = showRatioOfSprays (showTwoQSpraysX1X2X3 letter)

-- | Prints a ratio of sprays 
showRatioOfSpraysXYZ :: forall a. (Eq a, AlgField.C a) 
  => [String]         -- ^ typically some letters, to represent the variables
  -> (a -> String)    -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String) -- ^ used to enclose the coefficients, usually a pair of braces
  -> (String, String) -- ^ pair of braces to enclose the numerator and the denominator
  -> String           -- ^ represents the quotient bar
  -> RatioOfSprays a 
  -> String
showRatioOfSpraysXYZ letters showCoef coeffBraces = 
  showRatioOfSprays (showTwoSpraysXYZ showCoef coeffBraces letters)

-- | Prints a ratio of sprays 
showRatioOfSpraysXYZ' :: (Eq a, AlgField.C a)
  => [String]         -- ^ typically some letters, to represent the variables
  -> (a -> String)    -- ^ function mapping a coefficient to a string, typically 'show'
  -> RatioOfSprays a
  -> String
showRatioOfSpraysXYZ' letters showCoef = 
  showRatioOfSpraysXYZ letters showCoef ("(", ")") ("[ ", " ]") " %//% "

-- | Prints a ratio of sprays 
showRatioOfSpraysX1X2X3 :: forall a. (Eq a, AlgField.C a) 
  => String           -- ^ typically a letter, to represent the variables
  -> (a -> String)    -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String) -- ^ used to enclose the coefficients, usually a pair of braces
  -> (String, String) -- ^ pair of braces to enclose the numerator and the denominator
  -> String           -- ^ represents the quotient bar
  -> RatioOfSprays a 
  -> String
showRatioOfSpraysX1X2X3 letter showCoef coeffBraces = 
  showRatioOfSprays (showTwoSpraysX1X2X3 showCoef coeffBraces letter)

-- | Prints a ratio of sprays 
showRatioOfSpraysX1X2X3' :: (Eq a, AlgField.C a)
  => String          -- ^ typically a letter, to represent the variables
  -> (a -> String)   -- ^ function mapping a coefficient to a string, typically 'show'
  -> RatioOfSprays a
  -> String
showRatioOfSpraysX1X2X3' letter showCoef = 
  showRatioOfSpraysX1X2X3 letter showCoef ("(", ")") ("[ ", " ]") " %//% "

-- | Prints a ratio of sprays with rational coefficients
prettyRatioOfQSpraysXYZ :: 
     [String]         -- ^ typically some letters, to represent the variables
  -> RatioOfQSprays
  -> String
prettyRatioOfQSpraysXYZ letters = 
  showRatioOfQSpraysXYZ letters ("[ ", " ]") " %//% "

-- | Prints a ratio of sprays with rational coefficients
--
-- prop> prettyRatioOfQSprays rOS == prettyRatioOfQSpraysXYZ ["x","y","z"] rOS
prettyRatioOfQSprays :: RatioOfQSprays -> String
prettyRatioOfQSprays = prettyRatioOfQSpraysXYZ ["x", "y", "z"]

-- | Prints a ratio of sprays with rational coefficients
--
-- prop> prettyRatioOfQSprays' rOS == prettyRatioOfQSpraysXYZ ["X","Y","Z"] rOS
prettyRatioOfQSprays' :: RatioOfQSprays -> String
prettyRatioOfQSprays' = prettyRatioOfQSpraysXYZ ["X", "Y", "Z"]

-- | Prints a ratio of sprays with rational coefficients, printing the monomials 
-- in the style of @\"x1^2.x2.x3^3\"@
prettyRatioOfQSpraysX1X2X3 :: 
     String         -- ^ typically a letter, to represent the non-indexed variables
  -> RatioOfQSprays
  -> String
prettyRatioOfQSpraysX1X2X3 letter = 
  showRatioOfQSpraysX1X2X3 letter ("[ ", " ]") " %//% "

-- | Prints a ratio of sprays with numeric coefficients
prettyRatioOfNumSpraysXYZ :: (Num a, Ord a, AlgRing.C a, Show a)
  => [String]         -- ^ typically some letters, to represent the variables
  -> RatioOfSprays a
  -> String
prettyRatioOfNumSpraysXYZ letters = 
  showRatioOfNumSpraysXYZ show letters ("[ ", " ]") " %//% "

-- | Prints a ratio of sprays with numeric coefficients
--
-- prop> prettyRatioOfNumSprays rOS == prettyRatioOfNumSpraysXYZ ["x","y","z"] rOS
prettyRatioOfNumSprays :: 
  (Num a, Ord a, AlgRing.C a, Show a) => RatioOfSprays a -> String
prettyRatioOfNumSprays = prettyRatioOfNumSpraysXYZ ["x", "y", "z"]

-- | Prints a ratio of sprays with numeric coefficients
--
-- prop> prettyRatioOfNumSprays' rOS == prettyRatioOfNumSpraysXYZ ["X","Y","Z"] rOS
prettyRatioOfNumSprays' :: 
  (Num a, Ord a, AlgRing.C a, Show a) => RatioOfSprays a -> String
prettyRatioOfNumSprays' = prettyRatioOfNumSpraysXYZ ["X", "Y", "Z"]

-- | Prints a ratio of sprays with numeric coefficients, printing the monomials 
-- in the style of @\"x1^2.x2.x3^3\"@
prettyRatioOfNumSpraysX1X2X3 :: (Num a, Ord a, AlgRing.C a, Show a)
  => String          -- ^ typically a letter, to represent the variables
  -> RatioOfSprays a
  -> String
prettyRatioOfNumSpraysX1X2X3 letter = 
  showRatioOfNumSpraysX1X2X3 show letter ("[ ", " ]") " %//% "


-- Parametric sprays ----------------------------------------------------------

type SimpleParametricSpray a = Spray (Spray a)
type SimpleParametricQSpray  = SimpleParametricSpray Rational
type ParametricSpray a = Spray (RatioOfSprays a)
type ParametricQSpray  = ParametricSpray Rational

instance (Eq a, AlgRing.C a) => AlgMod.C a (SimpleParametricSpray a) where
  (*>) :: a -> SimpleParametricSpray a -> SimpleParametricSpray a
  lambda *> pspray = HM.map (lambda AlgMod.*>) pspray

instance (Eq a, AlgRing.C a) => AlgRightMod.C a (SimpleParametricSpray a) where
  (<*) :: SimpleParametricSpray a -> a -> SimpleParametricSpray a
  pspray <* lambda = HM.map (AlgRightMod.<* lambda) pspray

instance (Eq a, AlgField.C a) => AlgMod.C a (ParametricSpray a) where
  (*>) :: a -> ParametricSpray a -> ParametricSpray a
  lambda *> pspray = HM.map (lambda AlgMod.*>) pspray

instance (Eq a, AlgField.C a) => AlgRightMod.C a (ParametricSpray a) where
  (<*) :: ParametricSpray a -> a -> ParametricSpray a
  pspray <* lambda = HM.map (AlgRightMod.<* lambda) pspray

instance (Eq a, AlgField.C a) => AlgMod.C (Spray a) (ParametricSpray a) where
  (*>) :: Spray a -> ParametricSpray a -> ParametricSpray a
  spray *> pspray = asRatioOfSprays spray *^ pspray

instance (Eq a, AlgField.C a) => AlgRightMod.C (Spray a) (ParametricSpray a) where
  (<*) :: ParametricSpray a -> Spray a -> ParametricSpray a
  pspray <* spray = asRatioOfSprays spray *^ pspray

-- | Number of parameters in a parametric spray
--
-- >>> numberOfParameters (jacobiPolynomial 4)
-- 2
numberOfParameters :: HasVariables b => Spray b -> Int
numberOfParameters pspray = 
  if isZeroSpray pspray
    then 0
    else 
      maximum (map numberOfVariables (HM.elems pspray))

-- | Apply polynomial transformations to the parameters of a parametric spray; 
-- e.g. you have a two-parameters polynomial \(P_{a, b}(X, Y, Z)\) and you want
-- to get \(P_{a^2, b^2}(X, Y, Z)\), or the one-parameter polynomial 
-- \(P_{a, a}(X, Y, Z)\)
-- 
-- >>> jp = jacobiPolynomial 4
-- >>> a = qlone 1
-- >>> b = qlone 2
-- >>> changeParameters jp [a^**^2, b^**^2]
changeParameters :: 
  HasVariables b 
  => Spray b           -- ^ @OneParameterSpray a@, @SimpleParametricSpray a@, or @ParametricSpray a@
  -> [VariablesType b] -- ^ @[Polynomial a]@ or @[Spray a]@, the new variables 
  -> Spray b
changeParameters pspray newParameters = 
  if length newParameters < numberOfParameters pspray
    then 
      error "changeParameters: not enough new parameters provided."
    else 
      HM.map (`changeVariables` newParameters) pspray

-- | Substitutes some values to the parameters of a parametric spray
--
-- >>> jacobi3 = jacobiPolynomial 3
-- >>> legendre3 = substituteParameters jp [0, 0]
substituteParameters :: 
    (HasVariables b, Eq (BaseRing b), AlgAdd.C (BaseRing b)) 
  => Spray b            -- ^ @OneParameterSpray a@, @SimpleParametricSpray a@, or @ParametricSpray a@ 
  -> [BaseRing b]       -- ^ values of type @a@ to be substituted to the parameters
  -> Spray (BaseRing b) -- ^ output: a @Spray a@ spray
substituteParameters pspray values = 
  if length values < numberOfParameters pspray
    then 
      error "substituteParameters: not enough values provided."
    else 
      removeZeroTerms $ HM.map (evaluateAt values) pspray 

-- | helper function for evalParametricSpray
evalTerm' :: 
  (AlgMod.C (BaseRing b) b) => [BaseRing b] -> Term b -> b
evalTerm' xs (powers, coeff) = 
  AlgRing.product (zipWith (AlgRing.^) xs pows) AlgMod.*> coeff
  where 
    pows = DF.toList (fromIntegral <$> exponents powers)

-- | Substitutes some values to the variables of a parametric spray
evalParametricSpray ::
  (Eq b, AlgMod.C (BaseRing b) b, AlgRing.C b) 
  => Spray b            -- ^ @OneParameterSpray a@, @SimpleParametricSpray a@, or @ParametricSpray a@ 
  -> [BaseRing b]       -- ^ values of type @a@ to be substituted to the variables
  -> b
evalParametricSpray spray xs = if length xs >= numberOfVariables spray
  then AlgAdd.sum $ map (evalTerm' xs) (HM.toList spray)
  else error "evalParametricSpray: not enough values provided."

-- | Substitutes some values to the parameters of a parametric spray as well as 
-- some values to its variables
evalParametricSpray' ::
  (HasVariables b, Eq (BaseRing b), AlgMod.C (BaseRing b) b) 
  => Spray b      -- ^ @OneParameterSpray a@, @SimpleParametricSpray a@, or @ParametricSpray a@ 
  -> [BaseRing b] -- ^ values of type @a@ to be substituted to the parameters
  -> [BaseRing b] -- ^ values of type @a@ to be substituted to the variables
  -> BaseRing b   -- ^ result: a value of type @a@
evalParametricSpray' spray as xs = 
  evaluateAt xs (substituteParameters spray as)

-- | Whether the coefficients of a parametric spray polynomially 
-- depend on their parameters; I do not know why, but it seems to be the case 
-- for the Jacobi polynomials 
--
-- >>> canCoerceToSimpleParametricSpray (jacobiPolynomial 8)
-- True
canCoerceToSimpleParametricSpray :: 
  (Eq a, AlgRing.C a) => ParametricSpray a -> Bool
canCoerceToSimpleParametricSpray spray = 
  all isPolynomialRatioOfSprays (HM.elems spray)

-- | Coerces a parametric spray to a simple parametric spray, without 
-- checking this makes sense with `canCoerceToSimpleParametricSpray`
asSimpleParametricSprayUnsafe :: ParametricSpray a -> SimpleParametricSpray a
asSimpleParametricSprayUnsafe = HM.map _numerator

-- | Coerces a parametric spray to a simple parametric spray, after
-- checking this makes sense with `canCoerceToSimpleParametricSpray`
asSimpleParametricSpray :: 
  (Eq a, AlgRing.C a) => ParametricSpray a -> SimpleParametricSpray a
asSimpleParametricSpray spray = 
  if canCoerceToSimpleParametricSpray spray 
    then asSimpleParametricSprayUnsafe spray
    else error $
      "asSimpleParametricSpray: this parametric spray is not coercible" ++ 
      " to a simple parametric spray."

-- | Converts a `OneParameterSpray a` spray to a `ParametricSpray a`
fromOneParameterSpray :: 
  (Eq a, AlgRing.C a) => OneParameterSpray a -> ParametricSpray a
fromOneParameterSpray = HM.map fromRatioOfPolynomials

-- | Converts a `OneParameterQSpray` spray to a `ParametricQSpray`
fromOneParameterQSpray :: OneParameterQSpray -> ParametricQSpray
fromOneParameterQSpray = HM.map fromRatioOfQPolynomials

-- | Converts a `SimpleParametricSpray a` spray to a `ParametricSpray a`
fromSimpleParametricSpray :: 
  AlgRing.C a => SimpleParametricSpray a -> ParametricSpray a
fromSimpleParametricSpray = HM.map asRatioOfSprays

-- | Converts a parametric spray to a one-parameter spray, without checking
-- the conversion makes sense
parametricSprayToOneParameterSpray :: 
  forall a. (Eq a, AlgField.C a) => ParametricSpray a -> OneParameterSpray a
parametricSprayToOneParameterSpray = HM.map toRatioOfPolynomials
  where
    toRatioOfPolynomials :: RatioOfSprays a -> RatioOfPolynomials a
    toRatioOfPolynomials (RatioOfSprays p q) = 
      toPolynomial p % toPolynomial q
      where
        toPolynomial :: Spray a -> Polynomial a
        toPolynomial spray = polyFromCoeffs coeffs
          where
            coeffs = getConstantTerm spray : 
              [getCoefficient' (Powers (S.singleton i) 1) spray' 
                | i <- [1 .. deg]]
            deg = maximum (0 : expnts)
            spray' = removeConstantTerm spray
            expnts = map ((`index` 0) . exponents) (HM.keys spray')

{- -- | division of two univariate sprays
longDivision :: (Eq a, AlgField.C a) => Spray a -> Spray a -> (Spray a, Spray a)
longDivision sprayA sprayB = both fromCoeffs (polydiv coeffsA coeffsB)
  where
    fromCoeffs as = if S.null as
      then zeroSpray 
      else sumTerms terms
      where
        l = S.length as
        terms = (Powers S.empty 0, as `index` (l-1)) :
          map (\i -> (Powers (S.singleton i) 1, as `index` (l-1-i))) [1 .. l-1]
    shift n l = l >< S.replicate n AlgAdd.zero
    pad n l = if n > 0 then S.replicate n AlgAdd.zero >< l else l
    zipWith' op xs ys = S.zipWith op (pad (-d) xs) (pad d ys)
      where d = S.length xs - S.length ys
    coeffsA = coefficientsUnivariateSpray sprayA
    coeffsB = coefficientsUnivariateSpray sprayB
    coefficientsUnivariateSpray spray = coeffs
      where
        coeffs = S.fromList [getCoefficient' (Powers (S.singleton i) 1) spray' | 
                  i <- [deg, deg-1 .. 1]] |> getConstantTerm spray
        deg = maximum (0 : expnts)
        spray' = removeConstantTerm spray
        expnts = map ((`index` 0) . exponents) (HM.keys spray')
    polydiv as bs = aux as bs S.empty
      where aux f s q | ddif < 0 = (q, f)
                      | otherwise = aux f' s q'
              where ddif = S.length (norm f) - S.length (norm s)
                    k = f `index` 0 AlgField./ s `index` 0
                    ks = (AlgRing.* k) <$> shift ddif s
                    q' = zipWith' (AlgAdd.+) q $ shift ddif (S.singleton k)
                    f' = norm $ S.drop 1 $ zipWith' (AlgAdd.-) f ks
                    norm = S.dropWhileL (== AlgAdd.zero)  
 -}

-- | Converts a rational parametric spray to a rational one-parameter spray, 
-- without checking the conversion makes sense
parametricQSprayToOneParameterQSpray :: ParametricQSpray -> OneParameterQSpray
parametricQSprayToOneParameterQSpray = HM.map toRatioOfQPolynomials
  where
    toRatioOfQPolynomials :: RatioOfQSprays -> RatioOfQPolynomials
    toRatioOfQPolynomials (RatioOfSprays p q) = 
      toQPolynomial p % toQPolynomial q
      where
        toQPolynomial :: QSpray -> QPolynomial
        toQPolynomial spray = polyFromCoeffs coeffs'
          where
            coeffs' = f (getConstantTerm spray) : 
              [f $ getCoefficient' (Powers (S.singleton i) 1) spray' 
                | i <- [1 .. deg]]
            f :: Rational -> Rational'
            f r = DR.numerator r :% DR.denominator r
            deg = maximum (0 : map (`index` 0) expnts)
            spray' = removeConstantTerm spray
            expnts = map exponents (HM.keys spray')

-- | [Gegenbauer polynomials](https://en.wikipedia.org/wiki/Gegenbauer_polynomials); 
-- we mainly provide them to give an example of the @SimpleParametricSpray@ type
--
-- >>> gp = gegenbauerPolynomial 3
-- >>> putStrLn $ prettySimpleParametricQSpray gp
-- { (4/3)*a^3 + 4*a^2 + (8/3)*a }*X^3 + { -2*a^2 - 2*a }*X
-- >>> putStrLn $ prettyQSpray'' $ substituteParameters gp [1]
-- 8*X^3 - 4*X
gegenbauerPolynomial :: Int -> SimpleParametricQSpray 
gegenbauerPolynomial n 
  | n == 0 = unitSpray
  | n == 1 = (2.^a) *^ x
  | otherwise = 
    (2.^(n'' ^+^ a) /^ n') *^ (x ^*^ gegenbauerPolynomial (n - 1)) ^-^
      ((n'' ^+^ 2.^a ^-^ unitSpray) /^ n') *^ gegenbauerPolynomial (n - 2)
  where 
    x = lone 1 :: SimpleParametricQSpray
    a = lone 1 :: QSpray
    n'  = toRational n
    n'' = constantSpray (n' - 1)

-- | [Jacobi polynomial](https://en.wikipedia.org/wiki/Jacobi_polynomials); 
-- the @n@-th Jacobi polynomial is a univariate polynomial of degree @n@ with 
-- two parameters, except for the case @n=0@ where it has no parameter
--
-- >>> jP = jacobiPolynomial 1
-- >>> putStrLn $ prettyParametricQSprayABCXYZ ["alpha", "beta"] ["X"] jP
-- { [ (1/2)*alpha + (1/2)*beta + 1 ] }*X + { [ (1/2)*alpha - (1/2)*beta ] }
jacobiPolynomial :: Int -> ParametricQSpray
jacobiPolynomial n 
  | n < 0  = error "jacobiPolynomial: `n` must be positive." 
  | n == 0 = unitSpray
  | n == 1 = 
      fromSimpleParametricSpray $   
        (((gamma0 +> 2) /^ 2) *^ 
          (x +> AlgAdd.negate AlgRing.one)) +> (alpha0 +> 1)
  | otherwise = 
      spray ^*^ jacobiPolynomial (n-1) ^-^ rOS *^ jacobiPolynomial (n-2)
  where
    -- there's a lot of additions with a constant so we introduce 
    -- an operator to do them more efficiently
    (+>) :: (Eq a, AlgAdd.C a) => Spray a -> a -> Spray a
    (+>) q r = addTerm q (Powers S.empty 0, r)
    alpha0 = qlone 1
    beta0  = qlone 2
    gamma0 = alpha0 ^+^ beta0
    x = lone 1 :: SimpleParametricQSpray
    n' = toRational n
    a0 = alpha0 +> (n' - 1)
    b0 = beta0 +> (n' - 1)
    c0 = gamma0 +> (2 * n')
    c0' = c0 +> (-1)
    c0'' = c0 +> (-2)
    divisor = (n' *^ (c0 +> (-n'))) ^*^ c0''
    divisor' = 2 .^ divisor
    divide = (`RatioOfSprays` divisor')
    spray = HM.fromList [
        (
          Powers S.empty 0
        , divide $ c0' ^*^ (alpha0 ^-^ beta0) ^*^ gamma0
        ),
        (
          Powers (S.singleton 1) 1
        , divide $ c0' ^*^ c0 ^*^ c0''
        )
      ]
    rOS = RatioOfSprays (a0 ^*^ b0 ^*^ c0) divisor

-- | Pretty form of a numeric parametric spray, using some given strings (typically some 
-- letters) to denote the parameters and some given strings (typically some letters) to 
-- denote the variables; rather use `prettyParametricQSprayABCXYZ` for a rational 
-- parametric spray
prettyParametricNumSprayABCXYZ ::
  (Num a, Ord a, Show a, AlgField.C a)
  => [String]           -- ^ usually some letters, to denote the parameters of the spray
  -> [String]           -- ^ usually some letters, to denote the variables of the spray
  -> ParametricSpray a  -- ^ a parametric numeric spray
  -> String 
prettyParametricNumSprayABCXYZ abc xyz spray = 
  showSpray rOSShower ("{ ", " }") (showMonomialsXYZ xyz) spray
  where
    rOSShower = if numberOfParameters spray <= length abc
      then prettyRatioOfNumSpraysXYZ abc
      else prettyRatioOfNumSpraysX1X2X3 (abc !! 0)

-- | Pretty form of a numeric parametric spray; rather use `prettyParametricQSpray` for 
-- a rational parametric spray
--
-- prop> prettyParametricNumSpray == prettyParametricNumSprayABCXYZ ["a"] ["X","Y","Z"]
prettyParametricNumSpray ::
  (Num a, Ord a, Show a, AlgField.C a)
  => ParametricSpray a  -- ^ a parametric numeric spray
  -> String 
prettyParametricNumSpray = prettyParametricNumSprayABCXYZ ["a"] ["X", "Y", "Z"]

-- | Pretty form of a parametric rational spray, using some given strings (typically some 
-- letters) to denote the parameters and some given strings (typically some letters) to 
-- denote the variables
--
-- >>> type PQS = ParametricQSpray
-- >>> :{
-- >>> f :: (QSpray, QSpray) -> (PQS, PQS, PQS) -> PQS
-- >>> f (a, b) (x, y, z) = 
-- >>>   (a %:% (a ^+^ b)) *^ x^**^2  ^+^  (b %:% (a ^+^ b)) *^ (y ^*^ z)
-- >>> :}
-- >>> a = qlone 1
-- >>> b = qlone 2
-- >>> x = lone 1 :: PQS
-- >>> y = lone 2 :: PQS
-- >>> z = lone 3 :: PQS
-- >>> pqs = f (a, b) (x, y, z)
-- >>> putStrLn $ prettyParametricQSprayABCXYZ ["a","b"] ["X","Y","Z"] pqs
-- { [ a ] %//% [ a + b ] }*X^2 + { [ b ] %//% [ a + b ] }*Y.Z
prettyParametricQSprayABCXYZ ::
     [String]           -- ^ usually some letters, to denote the parameters of the spray
  -> [String]           -- ^ usually some letters, to denote the variables of the spray
  -> ParametricQSpray   -- ^ a parametric rational spray
  -> String 
prettyParametricQSprayABCXYZ abc xyz spray = 
  showSpray rOSShower ("{ ", " }") (showMonomialsXYZ xyz) spray
  where
    rOSShower = if numberOfParameters spray <= length abc
      then prettyRatioOfQSpraysXYZ abc
      else prettyRatioOfQSpraysX1X2X3 (abc !! 0)

-- | Pretty form of a parametric rational spray
--
-- prop> prettyParametricQSpray == prettyParametricQSprayABCXYZ ["a"] ["X","Y","Z"]
prettyParametricQSpray :: ParametricQSpray -> String 
prettyParametricQSpray = prettyParametricQSprayABCXYZ ["a"] ["X", "Y", "Z"]

-- | Pretty form of a numeric simple parametric spray, using some given strings (typically some 
-- letters) to denote the parameters and some given strings (typically some letters) to 
-- denote the variables; rather use `prettySimpleParametricQSprayABCXYZ` for a rational 
-- simple parametric spray
prettySimpleParametricNumSprayABCXYZ ::
  (Num a, Ord a, Show a, AlgRing.C a)
  => [String]                 -- ^ usually some letters, to denote the parameters of the spray
  -> [String]                 -- ^ usually some letters, to denote the variables of the spray
  -> SimpleParametricSpray a  -- ^ a numeric simple parametric spray
  -> String 
prettySimpleParametricNumSprayABCXYZ abc xyz spray = 
  showSpray rOSShower ("{ ", " }") (showMonomialsXYZ xyz) spray
  where
    rOSShower = if numberOfParameters spray <= length abc
      then prettyNumSprayXYZ abc
      else prettyNumSprayX1X2X3 (abc !! 0)

-- | Pretty form of a numeric simple parametric spray; rather use 
-- `prettySimpleParametricQSpray` for a numeric simple parametric spray
--
-- prop> prettySimpleParametricNumSpray == prettySimpleParametricNumSprayABCXYZ ["a"] ["X","Y","Z"]
prettySimpleParametricNumSpray ::
  (Num a, Ord a, Show a, AlgRing.C a)
  => SimpleParametricSpray a  -- ^ a numeric simple parametric spray
  -> String 
prettySimpleParametricNumSpray = 
  prettySimpleParametricNumSprayABCXYZ ["a"] ["X", "Y", "Z"]

-- | Pretty form of a simple parametric rational spray, using some given strings (typically some 
-- letters) to denote the parameters and some given strings (typically some letters) to 
-- denote the variables
--
-- >>> type SPQS = SimpleParametricQSpray
-- >>> :{
-- >>> f :: (QSpray, QSpray) -> (SPQS, SPQS, SPQS) -> SPQS
-- >>> f (a, b) (x, y, z) = 
-- >>>   (a ^+^ b) *^ x^**^2  ^+^  (a^**^2 ^+^ b^**^2) *^ (y ^*^ z)
-- >>> :}
-- >>> a = qlone 1
-- >>> b = qlone 2
-- >>> x = lone 1 :: SPQS
-- >>> y = lone 2 :: SPQS
-- >>> z = lone 3 :: SPQS
-- >>> spqs = f (a, b) (x, y, z)
-- >>> putStrLn $ prettySimpleParametricQSprayABCXYZ ["a","b"] ["X","Y","Z"] spqs
-- { a + b }*X^2 + { a^2 + b^2 }*Y.Z
prettySimpleParametricQSprayABCXYZ ::
     [String]               -- ^ usually some letters, to denote the parameters of the spray
  -> [String]               -- ^ usually some letters, to denote the variables of the spray
  -> SimpleParametricQSpray -- ^ a parametric rational spray
  -> String 
prettySimpleParametricQSprayABCXYZ abc xyz spray = 
  showSpray sprayShower ("{ ", " }") (showMonomialsXYZ xyz) spray
  where
    sprayShower = if numberOfParameters spray <= length abc
      then prettyQSprayXYZ abc
      else prettyQSprayX1X2X3 (abc !! 0)

-- | Pretty form of a simple parametric rational spray
--
-- prop> prettySimpleParametricQSpray == prettySimpleParametricQSprayABCXYZ ["a"] ["X","Y","Z"]
prettySimpleParametricQSpray :: SimpleParametricQSpray -> String 
prettySimpleParametricQSpray = 
  prettySimpleParametricQSprayABCXYZ ["a"] ["X", "Y", "Z"]
