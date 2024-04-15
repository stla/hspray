{-|
Module      : Math.Algebra.Hspray
Description : Multivariate polynomials on a ring.
Copyright   : (c) Stéphane Laurent, 2023
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

module Math.Algebra.Hspray
  ( 
  -- * Types
    Powers (..)
  , Spray
  , QSpray
  , QSpray'
  , Monomial
  -- * Basic sprays
  , lone
  , unitSpray
  , zeroSpray
  , constantSpray
  -- * Operations on sprays
  , (*^)
  , (.^)
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
  , (^/^)
  , prettyRatioOfPolynomials
  , prettyRatioOfQPolynomials
  , (*.)
  , constPoly
  , polyFromCoeffs
  , outerVariable
  , constQPoly
  , qpolyFromCoeffs
  , outerQVariable
  , evalRatioOfPolynomials
  -- * Symbolic sprays 
  , SymbolicSpray
  , SymbolicQSpray
  , prettySymbolicSprayX1X2X3
  , prettySymbolicSprayXYZ
  , prettySymbolicSpray
  , prettySymbolicSpray'
  , prettySymbolicQSpray
  , prettySymbolicQSpray'
  , prettySymbolicQSprayX1X2X3
  , prettySymbolicQSprayXYZ
  , simplifySymbolicSpray
  , evalSymbolicSpray
  , evalSymbolicSpray'
  , evalSymbolicSpray''
  -- * Queries on a spray
  , getCoefficient
  , getConstantTerm
  , numberOfVariables
  , sprayTerms
  -- * Evaluation of a spray
  , evalSpray
  , substituteSpray
  , composeSpray
  -- * Differentiation of a spray
  , derivSpray
  -- * Permutation of the variables of a spray
  , permuteVariables
  , swapVariables
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
  -- * Miscellaneous
  , fromList
  , toList
  , fromRationalSpray
  , leadingTerm
  , isPolynomialOf
  , bombieriSpray
  , collinearSprays
  ) where
import qualified Algebra.Additive              as AlgAdd
import qualified Algebra.Field                 as AlgField
import qualified Algebra.Module                as AlgMod
import qualified Algebra.Ring                  as AlgRing
import qualified Algebra.ZeroTestable          as AlgZT
import qualified Data.Foldable                 as DF
import           Data.Function                  ( on )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.Hashable                  ( Hashable(hashWithSalt) )
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
                                                , submatrix
                                                )
import qualified Data.Matrix                   as DM
import           Data.Maybe                     ( isJust
                                                , fromJust, fromMaybe
                                                )
import           Data.Ord                       ( comparing )
import qualified Data.Ratio                    as DR
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
import qualified MathObj.Polynomial            as MathPol
import           Number.Ratio                   ( T ( (:%) ) )
import qualified Number.Ratio                  as NumberRatio
-- import qualified Algebra.PrincipalIdealDomain  as AlgPID
-- import qualified Algebra.Units  as AlgUnits
-- import qualified Algebra.IntegralDomain  as AlgID


-- Univariate polynomials -----------------------------------------------------

newtype A a = A a 
  deriving
    (Eq, AlgAdd.C, AlgRing.C, AlgField.C)

type Rational' = NumberRatio.Rational
type Q = A Rational'

-- | Identify a rational to a @A Rational'@ element
scalarQ :: Rational' -> Q
scalarQ = A 

type Polynomial a         = MathPol.T (A a)
type RatioOfPolynomials a = NumberRatio.T (Polynomial a)
type QPolynomial          = Polynomial Rational'
type RatioOfQPolynomials  = RatioOfPolynomials Rational'

-- | Division of polynomials; this is an application of `:%` followed by 
-- a simplification of the fraction of the two polynomials
(^/^) :: (Eq a, AlgField.C a) 
      => Polynomial a -> Polynomial a -> RatioOfPolynomials a
(^/^) pol1 pol2 = simplifyRatioOfPolynomials $ pol1 :% pol2 

instance (Eq a, AlgField.C a) => AlgZT.C (A a) where
  isZero :: A a -> Bool
  isZero (A r) = r == AlgAdd.zero

instance (Eq a, AlgField.C a) => AlgMod.C (A a) (RatioOfPolynomials a) where
  (*>) :: A a -> RatioOfPolynomials a -> RatioOfPolynomials a
  r *> rop = NumberRatio.scale (MathPol.const r) rop 

instance (Eq a, AlgField.C a) => AlgMod.C (Polynomial a) (RatioOfPolynomials a) where
  (*>) :: Polynomial a -> RatioOfPolynomials a -> RatioOfPolynomials a
  p *> r = NumberRatio.scale p r 

instance (Eq a, AlgField.C a) => AlgMod.C (Polynomial a) (SymbolicSpray a) where
  (*>) :: Polynomial a -> SymbolicSpray a -> SymbolicSpray a
  p *> r = constantSpray (p NumberRatio.:% AlgRing.one) ^*^ r

infixr 7 *.
-- | Scale a ratio of univariate polynomials by a scalar
(*.) :: (Eq a, AlgField.C a) => a -> RatioOfPolynomials a -> RatioOfPolynomials a
(*.) scalar rop = A scalar AlgMod.*> rop

-- | Constant univariate polynomial
constPoly :: a -> Polynomial a
constPoly x = MathPol.const (A x)

-- | Univariate polynomial from its coefficients (ordered by increasing degrees)
polyFromCoeffs :: [a] -> Polynomial a
polyFromCoeffs as = MathPol.fromCoeffs (map A as)

-- | The variable of a univariate polynomial; it is called \"outer\" because this is the variable 
-- occuring in the polynomial coefficients of a `SymbolicSpray` 
outerVariable :: AlgRing.C a => Polynomial a
outerVariable = polyFromCoeffs [AlgAdd.zero, AlgRing.one] 

-- | Constant rational univariate polynomial
-- 
-- >>> import Number.Ratio ( (%) )
-- >>> constQPoly (2 % 3)
constQPoly :: Rational' -> QPolynomial
constQPoly = constPoly

-- | Rational univariate polynomial from coefficients
-- 
-- >>> import Number.Ratio ( (%) )
-- >>> qpolyFromCoeffs [2 % 3, 5, 7 % 4]
qpolyFromCoeffs :: [Rational'] -> QPolynomial
qpolyFromCoeffs = polyFromCoeffs

-- | The variable of a univariate qpolynomial; it is called \"outer\" because this is the variable 
-- occuring in the polynomial coefficients of a `SymbolicQSpray` 
--
-- prop> outerQVariable == qpolyFromCoeffs [0, 1] 
outerQVariable :: QPolynomial
outerQVariable = qpolyFromCoeffs [0, 1] 

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

-- | same as showQ with parentheses
showRatio' :: (Eq a, Num a, Show a) => NumberRatio.T a -> String
showRatio' q = if d == 1 
  then show n 
  else "(" ++ show n ++ "/" ++ show d ++ ")"
  where
    n = NumberRatio.numerator q
    d = NumberRatio.denominator q 

-- | identify a `Polynomial a` to a `Spray a`, in order to use the show functions
polynomialToSpray :: forall a. (Eq a, AlgRing.C a) => Polynomial a -> Spray a
polynomialToSpray pol = AlgAdd.sum terms
  where
    coeffs   = MathPol.coeffs pol
    indices = findIndices (/= A AlgAdd.zero) coeffs
    get :: A a -> a
    get (A x) = x
    terms = map (\i -> get (coeffs!!i) *^ (lone 1 ^**^ i)) indices

-- | helper function for prettyRatioOfPolynomials (and prettySymbolicSpray)
showRatioOfPolynomials :: forall a. (Eq a, AlgField.C a) 
                  => (Spray a -> String) -> RatioOfPolynomials a -> String
showRatioOfPolynomials sprayShower polysRatio = 
  numeratorString ++ denominatorString
  where
    numerator         = NumberRatio.numerator polysRatio
    denominator       = NumberRatio.denominator polysRatio
    brackets          = denominator /= MathPol.const (A AlgRing.one)
    enclose x = "[ " ++ x ++ " ]"
    numeratorString   = if brackets
      then enclose (sprayShower (polynomialToSpray numerator))
      else sprayShower (polynomialToSpray numerator)
    denominatorString = if not brackets
      then ""
      else " %//% " ++ enclose (sprayShower (polynomialToSpray denominator))

-- | Pretty form of a ratio of univariate polynomials with rational coefficients
prettyRatioOfQPolynomials
  :: String               -- ^ a string to denote the variable, e.g. @"a"@ 
  -> RatioOfQPolynomials 
  -> String 
prettyRatioOfQPolynomials var = showRatioOfPolynomials (prettyQSprayXYZ' [var])

-- | helper function for prettyRatioOfPolynomials (and prettySymbolicSpray)
showQpol :: forall a. (Eq a, AlgField.C a) 
         => Polynomial a -> String -> (a -> String) -> Bool -> String
showQpol pol variable showCoeff brackets = if brackets 
  then "[ " ++ polyString ++ " ]"
  else polyString
  where
    showCoeff' :: Int -> A a -> String
    showCoeff' i (A coeff) = case i of 
      0 -> '(' : showCoeff coeff ++ ")"
      _ -> if coeff == AlgRing.one 
        then "" 
        else '(' : showCoeff coeff ++ ")"
    coeffs   = MathPol.coeffs pol
    nonzeros = findIndices (/= A AlgAdd.zero) coeffs
    terms    = map (pack . showTerm) nonzeros
      where
        showTerm i = case i of 
          0 -> showCoeff' 0 (coeffs !! 0)
          1 -> showCoeff' 1 (coeffs !! 1) ++ variable
          _ -> showCoeff' i (coeffs !! i) ++ variable ++ "^" ++ show i
    polyString = unpack (intercalate (pack " + ") terms)

-- | helper function for prettyRatioOfPolynomials (and prettySymbolicSpray)
showQpolysRatio :: forall a. (Eq a, AlgField.C a) 
                   => String -> (a -> String) -> RatioOfPolynomials a -> String
showQpolysRatio var showCoeff polysRatio = numeratorString ++ denominatorString
  where
    denominator       = NumberRatio.denominator polysRatio
    brackets          = denominator /= MathPol.const (A AlgRing.one)
    numeratorString   = 
      showQpol (NumberRatio.numerator polysRatio) var showCoeff brackets
    denominatorString = if not brackets
      then ""
      else " %//% " ++ showQpol denominator var showCoeff True

-- | Pretty form of a ratio of univariate polynomials
prettyRatioOfPolynomials :: (Eq a, AlgField.C a, Show a) 
  => String               -- ^ a string to denote the variable, e.g. @"a"@
  -> RatioOfPolynomials a 
  -> String 
prettyRatioOfPolynomials var = showQpolysRatio var show 

{- -- | Pretty form of a ratio of univariate qpolynomials
prettyRatioOfQPolynomials' 
  :: String               -- ^ a string to denote the variable, e.g. @"a"@ 
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


-- Symbolic sprays ------------------------------------------------------------

type SymbolicSpray a = Spray (RatioOfPolynomials a)
type SymbolicQSpray  = SymbolicSpray Rational'

-- | simplifies ratio of polynomials
simplifyRatioOfPolynomials :: 
  (Eq a, AlgField.C a) => RatioOfPolynomials a -> RatioOfPolynomials a
simplifyRatioOfPolynomials = (AlgRing.*) AlgRing.one

-- | Simplifies the coefficients (the ratio of univariate polynomials) of a 
-- symbolic spray
simplifySymbolicSpray :: 
  (Eq a, AlgField.C a) => SymbolicSpray a -> SymbolicSpray a
simplifySymbolicSpray = HM.map simplifyRatioOfPolynomials

bracify :: (String, String) -> String -> String
bracify (lbrace, rbrace) x = lbrace ++ x ++ rbrace 

-- | Pretty form of a symbolic spray
prettySymbolicSprayX1X2X3 
  :: (Eq a, Show a, AlgField.C a) 
  => String          -- ^ string to denote the outer variable of the spray, e.g. @"a"@
  -> String          -- ^ typically a letter, to denote the non-indexed variables
  -> SymbolicSpray a -- ^ a symbolic spray; note that this function does not simplify it
  -> String 
prettySymbolicSprayX1X2X3 a = showSprayX1X2X3 (prettyRatioOfPolynomials a) ("{ ", " }")

-- | Pretty form of a symbolic spray
prettySymbolicSprayXYZ 
  :: (Eq a, Show a, AlgField.C a) 
  => String          -- ^ string to denote the outer variable of the spray, e.g. @"a"@
  -> [String]        -- ^ typically some letters, to denote the variables
  -> SymbolicSpray a -- ^ a symbolic spray; note that this function does not simplify it
  -> String 
prettySymbolicSprayXYZ a = showSprayXYZ (prettyRatioOfPolynomials a) ("{ ", " }")

-- | Pretty form of a symbolic spray
--
-- prop> prettySymbolicSpray a spray == prettySymbolicSprayXYZ a ["x", "y", "z"] spray
prettySymbolicSpray
  :: (Eq a, Show a, AlgField.C a) 
  => String          -- ^ string to denote the outer variable of the spray, e.g. @"a"@
  -> SymbolicSpray a -- ^ a symbolic spray; note that this function does not simplify it
  -> String 
prettySymbolicSpray a = prettySymbolicSprayXYZ a ["x", "y", "z"]

-- | Pretty form of a symbolic spray
--
-- prop> prettySymbolicSpray' a spray == prettySymbolicSprayXYZ a ["X", "Y", "Z"] spray
prettySymbolicSpray'
  :: (Eq a, Show a, AlgField.C a) 
  => String          -- ^ string to denote the outer variable of the spray, e.g. @"a"@
  -> SymbolicSpray a -- ^ a symbolic spray; note that this function does not simplify it
  -> String 
prettySymbolicSpray' a = prettySymbolicSprayXYZ a ["X", "Y", "Z"]

-- | Pretty form of a symbolic qspray
prettySymbolicQSprayX1X2X3 
  :: String          -- ^ string to denote the outer variable of the spray, e.g. @"a"@
  -> String          -- ^ string to denote the non-indexed variables of the spray
  -> SymbolicQSpray  -- ^ a symbolic qspray; note that this function does not simplify it
  -> String 
prettySymbolicQSprayX1X2X3 a x = 
  showSpray (prettyRatioOfQPolynomials a) ("{ ", " }") (showMonomialsX1X2X3 x)

-- | Pretty form of a symbolic qspray
prettySymbolicQSprayXYZ 
  :: String          -- ^ string to denote the outer variable of the spray, e.g. @"a"@
  -> [String]        -- ^ strings, usually letters, to denote the variables of the spray
  -> SymbolicQSpray  -- ^ a symbolic qspray; note that this function does not simplify it
  -> String 
prettySymbolicQSprayXYZ a letters = 
  showSpray (prettyRatioOfQPolynomials a) ("{ ", " }") (showMonomialsXYZ letters)

-- | Pretty form of a symbolic qspray
--
-- prop> prettySymbolicQSpray a == prettySymbolicQSprayXYZ a ["x", "y", "z"]
prettySymbolicQSpray 
  :: String          -- ^ a string to denote the outer variable of the spray, e.g. @"a"@
  -> SymbolicQSpray  -- ^ a symbolic qspray; note that this function does not simplify it
  -> String 
prettySymbolicQSpray a = prettySymbolicQSprayXYZ a ["x", "y", "z"] 

-- | Pretty form of a symbolic qspray
--
-- prop> prettySymbolicQSpray' a = prettySymbolicQSprayXYZ a ["X", "Y", "Z"]
prettySymbolicQSpray' 
  :: String          -- ^ a string to denote the outer variable of the spray, e.g. @"a"@
  -> SymbolicQSpray  -- ^ a symbolic qspray; note that this function does not simplify it
  -> String 
prettySymbolicQSpray' a = prettySymbolicQSprayXYZ a ["X", "Y", "Z"] 

-- | Substitutes a value to the outer variable of a symbolic spray
evalSymbolicSpray :: AlgField.C a => SymbolicSpray a -> a -> Spray a
evalSymbolicSpray spray x = HM.map (evalRatioOfPolynomials x) spray 

-- | Substitutes a value to the outer variable of a symbolic spray as well 
-- as some values to the inner variables of this spray
evalSymbolicSpray' :: AlgField.C a 
  => SymbolicSpray a -- ^ symbolic spray to be evaluated
  -> a               -- ^ a value for the outer variable
  -> [a]             -- ^ some values for the inner variables 
  -> a
evalSymbolicSpray' spray x xs = if length xs >= numberOfVariables spray 
  then evalSpray (evalSymbolicSpray spray x) xs
  else error "evalSymbolicSpray': not enough values provided."

-- helper function for evalSymbolicSpray''
evalSymbolicMonomial :: (Eq a, AlgField.C a) 
  => [a] -> Monomial (RatioOfPolynomials a) -> RatioOfPolynomials a
evalSymbolicMonomial xs (powers, coeff) = 
  AlgRing.product (zipWith (AlgRing.^) xs pows) *. coeff
  where 
    pows = DF.toList (fromIntegral <$> exponents powers)

-- | Substitutes some values to the inner variables of a symbolic spray
evalSymbolicSpray'' 
  :: (Eq a, AlgField.C a) => SymbolicSpray a -> [a] -> RatioOfPolynomials a
evalSymbolicSpray'' spray xs = if length xs >= numberOfVariables spray
  then AlgAdd.sum $ map (evalSymbolicMonomial xs) (HM.toList spray)
  else error "evalSymbolicSpray'': not enough values provided."


-- Sprays ---------------------------------------------------------------------

data Powers = Powers
  { exponents  :: Seq Int
  , nvariables :: Int
  }
  deriving Show

instance Eq Powers where
  (==) :: Powers -> Powers -> Bool
  pows1 == pows2 = exponents pows1' == exponents pows2'
    where 
      (pows1', pows2') = harmonize (pows1, pows2)

instance Hashable Powers where
  hashWithSalt :: Int -> Powers -> Int
  hashWithSalt k pows = hashWithSalt k (exponents pows, nvariables pows)

-- | append trailing zeros
growSequence :: Seq Int -> Int -> Int -> Seq Int
growSequence s m n = s >< t where t = S.replicate (n - m) 0

growSequence' :: Int -> Seq Int -> Seq Int
growSequence' n s = growSequence s (S.length s) n

-- | append trailing zeros to get the same length
harmonize :: (Powers, Powers) -> (Powers, Powers)
harmonize (pows1, pows2) = (Powers e1' n, Powers e2' n)
 where
  e1            = exponents pows1
  e2            = exponents pows2
  n1            = nvariables pows1
  n2            = nvariables pows2
  (e1', e2', n) = if n1 < n2
    then (growSequence e1 n1 n2, e2, n2)
    else (e1, growSequence e2 n2 n1, n1)

-- | drop trailing zeros
simplifyPowers :: Powers -> Powers
simplifyPowers pows = Powers s (S.length s)
  where 
    s = dropWhileR (== 0) (exponents pows)

type Monomial a = (Powers, a)
type Spray a = HashMap Powers a
type QSpray = Spray Rational
type QSpray' = Spray Rational'

-- | addition of two sprays
addSprays :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
addSprays p q = cleanSpray $ HM.foldlWithKey' f p q
  where 
    f s powers coef = HM.insertWith (AlgAdd.+) powers coef s

-- | opposite spray
negateSpray :: AlgAdd.C a => Spray a -> Spray a
negateSpray = HM.map AlgAdd.negate

-- | scale a spray by a scalar
scaleSpray :: (AlgRing.C a, Eq a) => a -> Spray a -> Spray a
scaleSpray lambda p = cleanSpray $ HM.map (lambda AlgRing.*) p

-- | multiply two monomials
multMonomial :: AlgRing.C a => Monomial a -> Monomial a -> Monomial a
multMonomial (pows1, coef1) (pows2, coef2) = (pows, coef1 AlgRing.* coef2)
 where
  (pows1', pows2') = harmonize (pows1, pows2)
  expts            = S.zipWith (+) (exponents pows1') (exponents pows2')
  pows             = Powers expts (nvariables pows1')

-- | multiply two sprays
multSprays :: (AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
multSprays p q = cleanSpray $ HM.fromListWith (AlgAdd.+) prods
 where
  p'    = HM.toList p
  q'    = HM.toList q
  prods = [ multMonomial mp mq | mp <- p', mq <- q' ]

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

instance (AlgRing.C a, Eq a) => AlgRing.C (Spray a) where
  (*) :: Spray a -> Spray a -> Spray a
  p * q = multSprays p q
  one :: Spray a
  one   = lone 0

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
  then AlgRing.product (replicate n p)
  else error "(^**^): negative power of a spray is not allowed."

infixr 7 *^
-- | Scale a spray by a scalar
(*^) :: (AlgRing.C a, Eq a) => a -> Spray a -> Spray a
(*^) lambda pol = lambda AlgMod.*> pol

infixr 7 .^
-- | Scale a spray by an integer
--
-- prop> 3 .^ p == p ^+^ p ^+^ p
(.^) :: (AlgAdd.C a, Eq a) => Int -> Spray a -> Spray a
(.^) k pol = if k >= 0
  then AlgAdd.sum (replicate k pol)
  else AlgAdd.negate $ AlgAdd.sum (replicate (-k) pol)

-- | drop trailing zeros in the powers of a spray
simplifySpray :: Spray a -> Spray a
simplifySpray = HM.mapKeys simplifyPowers

-- | simplify powers and remove zero terms
cleanSpray :: (AlgAdd.C a, Eq a) => Spray a -> Spray a
cleanSpray p = HM.filter (/= AlgAdd.zero) (simplifySpray p)

-- | derivative of a monomial
derivMonomial :: AlgRing.C a => Int -> Monomial a -> Monomial a 
derivMonomial i (pows, coef) = if i' >= S.length expts 
  then (Powers S.empty 0, AlgAdd.zero)
  else (pows', coef')
   where
    i'     = i - 1
    expts  = exponents pows
    expt_i = expts `index` i'
    expts' = adjust (subtract 1) i' expts
    coef'  = AlgAdd.sum (replicate expt_i coef)
    pows'  = Powers expts' (nvariables pows) 

-- | Derivative of a spray
derivSpray 
  :: (AlgRing.C a, Eq a) 
  => Int     -- ^ index of the variable of differentiation (starting at 1)
  -> Spray a -- ^ the spray to be derivated
  -> Spray a -- ^ the derivated spray
derivSpray i p = if i >= 1 
  then cleanSpray $ HM.fromListWith (AlgAdd.+) monomials
  else error "derivSpray: invalid index."
 where
  p'        = HM.toList p
  monomials = [ derivMonomial i mp | mp <- p' ]

-- | Spray corresponding to the basic monomial x_n
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> p = 2*^x^**^2 ^-^ 3*^y
-- >>> putStrLn $ prettySpray' p
-- (2) x1^2 + (-3) x2
--
-- prop> lone 0 == unitSpray
lone :: AlgRing.C a => Int -> Spray a
lone n = if n >= 0 
  then HM.singleton pows AlgRing.one
  else error "lone: invalid index."
 where
  pows = if n == 0
    then Powers S.empty 0
    else Powers (S.replicate (n - 1) AlgAdd.zero |> AlgRing.one) n

-- | The unit spray
--
-- prop> p ^*^ unitSpray == p
unitSpray :: AlgRing.C a => Spray a
unitSpray = lone 0

-- | The null spray
--
-- prop> p ^+^ zeroSpray == p
zeroSpray :: (Eq a, AlgAdd.C a) => Spray a
zeroSpray = AlgAdd.zero

-- | Constant spray
--
-- prop> constantSpray 3 == 3 *^ unitSpray
constantSpray :: (AlgRing.C a, Eq a) => a -> Spray a
constantSpray c = c *^ lone 0

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
getCoefficient expnts spray = fromMaybe AlgAdd.zero (HM.lookup powers spray)
  where
    expnts' = S.dropWhileR (== 0) (S.fromList expnts)
    powers  = Powers expnts' (S.length expnts')

-- | Get the constant term of a spray
--
-- prop> getConstantTerm p == getCoefficient [] p 
getConstantTerm :: AlgAdd.C a => Spray a -> a
getConstantTerm spray = fromMaybe AlgAdd.zero (HM.lookup powers spray)
  where
    powers  = Powers S.empty 0

-- | number of variables in a spray
numberOfVariables :: Spray a -> Int
numberOfVariables spray =
  if null powers then 0 else maximum (map nvariables powers)
  where
    powers = HM.keys spray

-- | evaluates a monomial
evalMonomial :: AlgRing.C a => [a] -> Monomial a -> a
evalMonomial xyz (powers, coeff) = 
  coeff AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows)
  where 
    pows = DF.toList (fromIntegral <$> exponents powers)

-- | Evaluates a spray
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> p = 2*^x^**^2 ^-^ 3*^y
-- >>> evalSpray p [2, 1]
-- 5
evalSpray :: AlgRing.C a => Spray a -> [a] -> a
evalSpray p xyz = if length xyz >= numberOfVariables p
  then AlgAdd.sum $ map (evalMonomial xyz) (HM.toList p)
  else error "evalSpray: not enough values provided."

-- | spray from monomial
fromMonomial :: Monomial a -> Spray a
fromMonomial (pows, coeff) = HM.singleton pows coeff

-- | substitute some variables in a monomial
substituteMonomial :: AlgRing.C a => [Maybe a] -> Monomial a -> Monomial a
substituteMonomial subs (powers, coeff) = (powers'', coeff')
  where
    pows     = exponents powers
    n        = nvariables powers
    indices  = findIndices isJust (take n subs)
    pows'    = [fromIntegral (pows `index` i) | i <- indices]
    xyz      = [fromJust (subs !! i) | i <- indices]
    coeff'   = coeff AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows')
    f i a    = if i `elem` indices then 0 else a
    pows''   = S.mapWithIndex f pows
    powers'' = simplifyPowers $ Powers pows'' n

-- | Substitutes some variables in a spray by some values
--
-- >>> x1 :: lone 1 :: Spray Int
-- >>> x2 :: lone 2 :: Spray Int
-- >>> x3 :: lone 3 :: Spray Int
-- >>> p = x1^**^2 ^-^ x2 ^+^ x3 ^-^ unitSpray
-- >>> p' = substituteSpray [Just 2, Nothing, Just 3] p
-- >>> putStrLn $ prettySpray' p'
-- (-1) x2 + (6) 
substituteSpray :: (Eq a, AlgRing.C a) => [Maybe a] -> Spray a -> Spray a
substituteSpray subs spray = if length subs == n 
  then spray'
  else error "substituteSpray: incorrect length of the substitutions list."
  where
    n         = numberOfVariables spray
    monomials = HM.toList spray
    spray'    = 
      foldl1' (^+^) (map (fromMonomial . substituteMonomial subs) monomials)

-- | Converts a spray with rational coefficients to a spray with double 
-- coefficients (useful for evaluation)
fromRationalSpray :: Spray Rational -> Spray Double
fromRationalSpray = HM.map fromRational

-- | Sustitutes the variables of a spray with some sprays 
-- (e.g. change of variables)
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = x ^+^ y
-- >>> q = composeSpray p [z, x ^+^ y ^+^ z]
-- >>> putStrLn $ prettySprayXYZ q
-- (1) X + (1) Y + (2) Z
composeSpray :: forall a. (AlgRing.C a, Eq a) 
                => Spray a -> [Spray a] -> Spray a
composeSpray p = evalSpray (identify p)
  where 
    identify :: Spray a -> Spray (Spray a)
    identify = HM.map constantSpray

-- | Creates a spray from a list of terms
fromList :: (AlgRing.C a, Eq a) => [([Int], a)] -> Spray a
fromList x = cleanSpray $ HM.fromList $ map
  (\(expts, coef) -> (Powers (S.fromList expts) (length expts), coef)) x

-- | Permutes the variables of a spray
--
-- >>> f :: Spray Rational -> Spray Rational -> Spray Rational -> Spray Rational
-- >>> f p1 p2 p3 = p1^**^4 ^+^ (2*^p2^**^3) ^+^ (3*^p3^**^2) ^-^ (4*^unitSpray)
-- >>> x1 = lone 1 :: Spray Rational
-- >>> x2 = lone 2 :: Spray Rational
-- >>> x3 = lone 3 :: Spray Rational
-- >>> p = f x1 x2 x3
--
-- prop> permuteVariables [3, 1, 2] p == f x3 x1 x2
permuteVariables :: [Int] -> Spray a -> Spray a
permuteVariables permutation spray = 
  if n' >= n && isPermutation permutation  
    then spray'
    else error "permuteVariables: invalid permutation."
  where
    n  = numberOfVariables spray
    n' = maximum permutation
    isPermutation pmtn = minimum pmtn == 1 && length (nub pmtn) == n'
    intmap         = IM.fromList (zip permutation [1 .. n'])
    invpermutation = [intmap IM.! i | i <- [1 .. n']]
    permuteSeq x   = 
      S.mapWithIndex (\i _ -> x `index` (invpermutation !! i - 1)) x 
    (powers, coeffs) = unzip (HM.toList spray)
    expnts  = map exponents powers
    expnts' = map (permuteSeq . growSequence' n') expnts
    powers' = map (\exps -> simplifyPowers (Powers exps n')) expnts'
    spray'  = HM.fromList (zip powers' coeffs)

-- | Swaps two variables of a spray
-- 
-- prop> swapVariables (1, 3) p == permuteVariables [3, 2, 1] p
swapVariables :: (Int, Int) -> Spray a -> Spray a
swapVariables (i, j) spray = 
  if i>=1 && j>=1  
    then spray'
    else error "swapVariables: invalid indices."
  where
    n = maximum [numberOfVariables spray, i, j]
    f k | k == i    = j
        | k == j    = i
        | otherwise = k
    transposition = map f [1 .. n]
    permuteSeq x  = 
      S.mapWithIndex (\ii _ -> x `index` (transposition !! ii - 1)) x 
    (powers, coeffs) = unzip (HM.toList spray)
    expnts  = map exponents powers
    expnts' = map (permuteSeq . growSequence' n) expnts
    powers' = map (\exps -> simplifyPowers (Powers exps n)) expnts'
    spray'  = HM.fromList (zip powers' coeffs)


-- pretty stuff ---------------------------------------------------------------

-- | Print a spray; this function is exported for 
-- possible usage in other packages
showSpray 
  :: (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String)        -- ^ pair of braces to enclose the coefficients
  -> ([Seq Int] -> [String]) -- ^ function mapping a list of exponents to a list of strings representing the monomials corresponding to these exponents
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSpray showCoef braces showMonomials p = 
  unpack $ intercalate (pack " + ") stringTerms
  where
    terms = sortBy (flip compare `on` fexpts) (HM.toList p)
    fexpts term     = exponents $ fst term
    coeffs = map snd terms
    powers = map (exponents . fst) terms
    stringMonomials = showMonomials powers
    stringTerms = zipWith f coeffs stringMonomials
    f coeff smonomial 
      | smonomial == "" = pack scoeff
      | scoeff' == ""   = pack smonomial
      | otherwise       = pack $ scoeff ++ "*" ++ smonomial
      where
        scoeff  = showCoef coeff
        scoeff' = bracify braces scoeff 

-- | Prints a spray, with monomials shown as "x.z^2", and with 
-- a user-defined showing function for the coefficients
showSprayXYZ 
  :: (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> (String, String)        -- ^ used to enclose the coefficients
  -> [String]                -- ^ strings, typically some letters, to print the variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSprayXYZ showCoef braces letters =
  showSpray showCoef braces (showMonomialsXYZ letters)

-- | Prints a spray, with monomials shown as @"x.z^2"@, and with 
-- a user-defined showing function for the coefficients; this is the same as 
-- the function `showSprayXYZ` with the pair of braces @("(", ")")@
showSprayXYZ' 
  :: (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
  -> [String]                -- ^ strings, typically some letters, to print the variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSprayXYZ' showCoef = showSprayXYZ showCoef ("(", ")")

-- | Pretty form of a spray with monomials displayed in the style of @"x.z^2"@; 
-- you should rather use `prettyNumSprayXYZ` or `prettyQSpprayXYZ` if your 
-- coefficients are numeric
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySprayXYZ ["X", "Y", "Z"] p
-- (2)*X + (3)*Y^2 + (-4)*Z^3
-- >>> putStrLn $ prettySprayXYZ ["X", "Y"] p
-- (2)*X1 + (3)*X2^2 + (-4)*X3^3
prettySprayXYZ :: (Show a) 
  => [String]                -- ^ typically some letters, to print the variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
prettySprayXYZ = showSprayXYZ' show
  
-- | Pretty form of a spray, with monomials shown as "x1.x3^2", and with 
-- a user-defined showing function for the coefficients
showSprayX1X2X3
  :: (a -> String)           -- ^ function mapping a coefficient to a string, typically 'show'
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
showSprayX1X2X3'
  :: (a -> String)           -- ^ function mapping a coefficient to a string, e.g. 'show'
  -> String                  -- ^ typically a letter, to print the non-indexed variables
  -> Spray a                 -- ^ the spray to be printed
  -> String
showSprayX1X2X3' showCoef = showSprayX1X2X3 showCoef ("(", ")")

-- | Pretty form of a spray with monomials displayed in the style of @"x.z^2"@; 
-- you should rather use `prettyNumSprayXYZ` or `prettyQSpprayXYZ` if your 
-- coefficients are numeric
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySprayXYZ ["X", "Y", "Z"] p
-- (2)*X + (3)*Y^2 + (-4)*Z^3
-- >>> putStrLn $ prettySprayXYZ ["X", "Y"] p
-- (2)*X1 + (3)*X2^2 + (-4)*X3^3
prettySprayX1X2X3 :: (Show a) 
  => String                -- ^ typically a letter, to print the non-indexed variables
  -> Spray a               -- ^ the spray to be printed
  -> String
prettySprayX1X2X3 = showSprayX1X2X3' show

-- | Pretty form of a spray with monomials displayed in the style of @"x.z^2"@; 
-- you should rather use `prettyNumSpray` or `prettyQSppray` if your 
-- coefficients are numeric
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray p
-- (2)*x + (3)*y^2 + (-4)*z^3
-- >>> putStrLn $ prettySpray (p ^+^ lone 4)
-- (2)*x1 + (3)*x2^2 + (-4)*x3^3 + x4
--
-- prop> prettySpray spray == prettySprayXYZ ["x", "y", "z"] spray
prettySpray :: (Show a) => Spray a -> String
prettySpray = prettySprayXYZ ["x", "y", "z"]

-- | Pretty form of a spray, with monomials shown as "x1.x3^2"; use 
-- `prettySprayX1X2X3` to change the letter (or `prettyNumSprayX1X2X3` 
-- or `prettyQSprayX1X2X3` if the coefficients are numeric)
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray' p
-- (2)*x1 + (3)*x2^2 + (-4)*x3^3 
prettySpray' :: Show a => Spray a -> String
prettySpray' = prettySprayX1X2X3 "x"

-- | showMonomial "x" [0, 2, 1] = x^(0, 2, 1)
showMonomialsOld :: String -> [Seq Int] -> [String]
showMonomialsOld var = map (showMonomialOld var) 
  where
    showMonomialOld :: String -> Seq Int -> String
    showMonomialOld a pows = 
      unpack $ append (pack x) (cons '(' $ snoc string ')')
      where
        x      = " " ++ a ++ "^"
        string = intercalate (pack ", ") (map (pack . show) (DF.toList pows))

-- | Pretty form of a spray; you will probably prefer 
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray'' "x" p
-- (2)*x^(1) + (3)*x^(0, 2) + (-4)*x^(0, 0, 3)
prettySpray'' 
  :: Show a 
  => String        -- ^ a string denoting the variables, e.g. \"x\"
  -> Spray a       -- ^ the spray
  -> String
prettySpray'' var = showSpray show ("(", ")") (showMonomialsOld var)

-- | Show a spray with numeric coefficients; this function is exported for 
-- possible usage in other packages
showNumSpray :: (Num a, Ord a)
  => ([Seq Int] -> [String]) -- ^ function mapping a list of monomials exponents to a list of strings
  -> (a -> String)           -- ^ function mapping a positive coefficient to a string
  -> Spray a
  -> String
showNumSpray showMonomials showCoeff spray = 
  if HM.size spray == 0 
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

-- | showMonomialsXYZ ["X", "Y", "Z"] [[0, 2, 1], [1, 2]] = ["Y^2.Z", "X.Y"]
showMonomialsXYZ :: [String] -> [Seq Int] -> [String]
showMonomialsXYZ letters powers = map (unpack . showMonomialXYZ letters n) powers
  where 
    n = maximum (map S.length powers)

-- | Pretty form of a spray with numeric coefficients, printing monomials as @"x1.x3^2"@
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettyNumSprayX1X2X3 "x" p
-- 2*x1 + 3*x2^2 - 4*x3^3 
prettyNumSprayX1X2X3 :: (Num a, Ord a, Show a)
  => String   -- ^ usually a letter such as @"x"@ to denote the non-indexed variables
  -> Spray a
  -> String
prettyNumSprayX1X2X3 x = showNumSpray (showMonomialsX1X2X3 x) show

-- | Pretty form of a spray with numeric coefficients, printing monomials as @"x.z^2"@
-- if possible, i.e. if enough letters are provided, otherwise as @"x1.x3^2"@
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> w :: lone 4 :: Spray Int
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

-- Print a `QSpray`; for internal usage but exported for usage in other packages
showQSpray :: 
   ([Seq Int] -> [String]) -- ^ function mapping a list of monomials exponents to a list of strings
  -> QSpray
  -> String
showQSpray showMonomials = showNumSpray showMonomials showRatio

-- Print a `QSpray'`; for internal usage but exported for usage in other packages
showQSpray' :: 
   ([Seq Int] -> [String]) -- ^ function mapping a list of monomials exponents to a list of strings
  -> QSpray'
  -> String
showQSpray' showMonomials = showNumSpray showMonomials showRatio'

-- | Pretty form of a spray with rational coefficients, printing monomials in 
-- the style of @"x1.x3^2"@
--
-- >>> x :: lone 1 :: QSpray
-- >>> y :: lone 2 :: QSpray
-- >>> z :: lone 3 :: QSpray
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ (4%3)*^z^**^3
-- >>> putStrLn $ prettyQSprayX1X2X3 "x" p
-- 2*x1 + 3*x2^2 - (4/3)*x3^3 
prettyQSprayX1X2X3 :: 
     String   -- ^ usually a letter such as @"x"@ to denote the non-indexed variables
  -> QSpray
  -> String
prettyQSprayX1X2X3 x = showQSpray (showMonomialsX1X2X3 x)

-- | Same as `prettyQSprayX1X2X3` but for `QSpray'`
prettyQSprayX1X2X3' :: 
     String   -- ^ usually a letter such as @"x"@ to denote the non-indexed variables
  -> QSpray'
  -> String
prettyQSprayX1X2X3' x = showQSpray' (showMonomialsX1X2X3 x)

-- | Pretty form of a spray with rational coefficients, printing monomials in 
-- the style of @"x.z^2"@ with the provided letters if possible, i.e. if enough 
-- letters are provided, otherwise in the style @"x1.x3^2"@, taking the first 
-- provided letter to denote the non-indexed variables
--
-- >>> x :: lone 1 :: QSpray
-- >>> y :: lone 2 :: QSpray
-- >>> z :: lone 3 :: QSpray
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
prettyQSprayXYZ letters = showNumSpray (showMonomialsXYZ letters) showRatio

-- | Same as `prettyQSprayXYZ` but for `QSpray'`
prettyQSprayXYZ' :: 
    [String]   -- ^ usually some letters, to denote the variables
  -> QSpray'
  -> String
prettyQSprayXYZ' letters = showNumSpray (showMonomialsXYZ letters) showRatio'

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
bombieriSpray :: AlgAdd.C a => Spray a -> Spray a
bombieriSpray = HM.mapWithKey f
 where
  f pows          = times (pfactorial $ exponents pows)
  pfactorial pows = product $ DF.toList $ factorial <$> S.filter (/= 0) pows
  factorial n     = product [1 .. n]
  times k x       = AlgAdd.sum (replicate k x)

-- | Whether two sprays are equal up to a scalar factor
collinearSprays :: (Eq a, AlgField.C a) => Spray a -> Spray a -> Bool
collinearSprays spray1 spray2 = r *^ spray2 == spray1
  where
    r = snd (leadingTerm spray1) AlgField./ snd (leadingTerm spray2)


-- division stuff -------------------------------------------------------------

-- | index of the maximum of a list
maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0 .. ]

-- | Leading term of a spray 
leadingTerm :: Spray a -> Monomial a
leadingTerm p = (biggest, p HM.! biggest) 
  where
    powers  = HM.keys p
    i       = maxIndex $ map exponents powers
    biggest = powers !! i

-- | whether a monomial divides another monomial
divides :: Monomial a -> Monomial a -> Bool
divides (powsP, _) (powsQ, _) = S.length expntsP <= S.length expntsQ && lower
  where
    expntsP = exponents powsP
    expntsQ = exponents powsQ
    lower   = DF.all (uncurry (<=)) (S.zip expntsP expntsQ)

-- | quotient of monomial Q by monomial p, assuming P divides Q
quotient :: AlgField.C a => Monomial a -> Monomial a -> Monomial a
quotient (powsQ, coeffQ) (powsP, coeffP) = (pows, coeff)
  where
    (powsP', powsQ') = harmonize (powsP, powsQ)
    expntsP          = exponents powsP'
    expntsQ          = exponents powsQ'
    expnts           = S.zipWith (-) expntsQ expntsP
    n                = nvariables powsP'
    pows             = Powers expnts n
    coeff            = coeffQ AlgField./ coeffP

-- | Remainder of the division of a spray by a list of divisors, 
-- using the lexicographic ordering of the monomials
sprayDivisionRemainder :: forall a. (Eq a, AlgField.C a) 
                          => Spray a -> [Spray a] -> Spray a
sprayDivisionRemainder p qs = 
  if n == 0 
    then error "sprayDivisionRemainder: the list of divisors is empty." 
    else snd $ ogo p AlgAdd.zero
  where
    n = length qs
    qsltqs = zip qs (map leadingTerm qs)
    g :: Monomial a -> Spray a -> Spray a -> (Spray a, Spray a)
    g lts s r = (s ^-^ ltsspray, r ^+^ ltsspray)
      where
        ltsspray = fromMonomial lts 
    go :: Monomial a -> Spray a -> Spray a -> Int -> Bool -> (Spray a, Spray a)
    go lts !s r !i !divoccured
      | divoccured = (s, r)
      | i == n     = g lts s r 
      | otherwise  = go lts news r (i+1) newdivoccured
        where
          (q, ltq)      = qsltqs !! i
          newdivoccured = divides ltq lts
          news          = if newdivoccured
            then s ^-^ (fromMonomial (quotient lts ltq) ^*^ q)
            else s
    ogo :: Spray a -> Spray a -> (Spray a, Spray a)
    ogo !s !r 
      | s == AlgAdd.zero = (s, r)
      | otherwise        = ogo s' r'
        where
          (s', r') = go (leadingTerm s) s r 0 False

-- | Division of a spray by a spray
sprayDivision :: forall a. (Eq a, AlgField.C a) 
  => Spray a            -- ^ dividend 
  -> Spray a            -- ^ divisor
  -> (Spray a, Spray a) -- ^ (quotient, remainder)
sprayDivision sprayA sprayB =
  if sprayB == AlgAdd.zero 
    then error "sprayDivision: division by zero."
    else ogo sprayA AlgAdd.zero AlgAdd.zero
  where
    go :: Monomial a -> Spray a -> Spray a -> Spray a -> Int -> Bool 
          -> (Spray a, Spray a, Spray a)
    go ltp !p !q r !i !divoccured
      | divoccured = (p, q, r)
      | i == 1     = (p ^-^ ltpspray, q, r ^+^ ltpspray)
      | otherwise  = go ltp newp newq r 1 newdivoccured
        where
          ltpspray      = fromMonomial ltp
          ltB           = leadingTerm sprayB
          newdivoccured = divides ltB ltp
          (newp, newq)  = if newdivoccured
            then (p ^-^ (qtnt ^*^ sprayB), q ^+^ qtnt)
            else (p, q)
            where
              qtnt = fromMonomial $ quotient ltp ltB
    ogo :: Spray a -> Spray a -> Spray a -> (Spray a, Spray a)
    ogo !p !q !r 
      | p == AlgAdd.zero = (q, r)
      | otherwise        = ogo p' q' r'
        where
          (p', q', r') = go (leadingTerm p) p q r 0 False


-- Groebner stuff -------------------------------------------------------------

-- | slight modification of `sprayDivisionRemainder` to speed up groebner00
sprayDivisionRemainder' 
  :: forall a. (Eq a, AlgField.C a) 
  => Spray a -> HashMap Int (Spray a, Monomial a) -> Spray a
sprayDivisionRemainder' p qsltqs = snd $ ogo p AlgAdd.zero
  where
    n = HM.size qsltqs
    g :: Monomial a -> Spray a -> Spray a -> (Spray a, Spray a)
    g lts s r = (s ^-^ ltsspray, r ^+^ ltsspray)
      where
        ltsspray = fromMonomial lts 
    go :: Monomial a -> Spray a -> Spray a -> Int -> Bool -> (Spray a, Spray a)
    go lts !s r !i !divoccured
      | divoccured = (s, r)
      | i == n     = g lts s r 
      | otherwise  = go lts news r (i+1) newdivoccured
        where
          (q, ltq)      = qsltqs HM.! i
          newdivoccured = divides ltq lts
          news = if newdivoccured
            then s ^-^ (fromMonomial (quotient lts ltq) ^*^ q)
            else s
    ogo :: Spray a -> Spray a -> (Spray a, Spray a)
    ogo !s !r 
      | s == AlgAdd.zero = (s, r)
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
               => (Spray a, Monomial a) -> (Spray a, Monomial a) -> Spray a
sPolynomial pltp qltq = wp ^*^ p ^-^ wq ^*^ q
  where
    p                 = fst pltp
    q                 = fst qltq
    (lpowsP, lcoefP)  = snd pltp
    (lpowsQ, lcoefQ)  = snd qltq
    (lpowsP', lpowsQ') = harmonize (lpowsP, lpowsQ)
    lexpntsP           = exponents lpowsP'
    lexpntsQ           = exponents lpowsQ'
    gamma = S.zipWith max lexpntsP lexpntsQ
    betaP = S.zipWith (-) gamma lexpntsP
    betaQ = S.zipWith (-) gamma lexpntsQ
    n  = nvariables lpowsP'
    wp = fromMonomial (Powers betaP n, AlgField.recip lcoefP)
    wq = fromMonomial (Powers betaQ n, AlgField.recip lcoefQ)

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
          -> HashMap Int (Spray a, Monomial a) -> [Spray a]
    go !i !j !combins !gpolysMap
      | i == length combins = map fst (HM.elems gpolysMap)
      | otherwise           = go i' j' combins' gpolysMap'
        where
          (k, l)   = combins HM.! i
          sfg      = sPolynomial (gpolysMap HM.! k) (gpolysMap HM.! l)
          sbarfg   = sprayDivisionRemainder' sfg gpolysMap
          ltsbarfg = leadingTerm sbarfg
          (i', j', gpolysMap', combins') = if sbarfg == AlgAdd.zero
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

-- | Reduces a Groebner basis
reduceGroebnerBasis :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
reduceGroebnerBasis gbasis = 
  if length gbasis >= 2 
    then map reduction [0 .. n-1] 
    else ngbasis
  where
    normalize :: Spray a -> Spray a
    normalize spray = AlgField.recip coef *^ spray
      where
        (_, coef) = leadingTerm spray
    ngbasis = map normalize gbasis
    n       = length ngbasis
    reduction :: Int -> Spray a
    reduction i = sprayDivisionRemainder (ngbasis !! i) rest
      where
        rest = [ngbasis !! k | k <- [0 .. n-1] \\ [i]]

-- | Groebner basis (always minimal and possibly reduced)
--
-- prop> groebner sprays True == reduceGroebnerBasis (groebner sprays False)
groebner 
  :: forall a. (Eq a, AlgField.C a) 
  => [Spray a] -- ^ list of sprays 
  -> Bool      -- ^ whether to return the reduced basis
  -> [Spray a]
groebner sprays reduced = 
  if reduced then reduceGroebnerBasis gbasis0 else map normalize gbasis0
  where
    gbasis0 = groebner0 sprays
    normalize :: Spray a -> Spray a
    normalize spray = AlgField.recip coef *^ spray
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
-- (1) x1x2 + (1) x1x3 + (1) x2x3
esPolynomial 
  :: (AlgRing.C a, Eq a) 
  => Int -- ^ number of variables
  -> Int -- ^ index
  -> Spray a
esPolynomial n k
  | k < 0 || n < 0 
    = error "esPolynomial: both arguments must be positive integers."
  | k > n     = AlgAdd.zero
  | k == 0    = unitSpray
  | otherwise = simplifySpray spray
  where
    perms = permutationsBinarySequence (n-k) k
    spray = HM.fromList $ map (\expts -> (Powers expts n, AlgRing.one)) perms

-- | Power sum polynomial
psPolynomial 
  :: forall a. (AlgRing.C a, Eq a) 
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
-- (use the function with the same name in the /jackpolynomials/ package 
-- if you need efficiency)
isSymmetricSpray :: forall a. (AlgField.C a, Eq a) => Spray a -> Bool
isSymmetricSpray spray = check1 && check2 
  where
    n = numberOfVariables spray
    indices = [1 .. n]
    gPolys = map (\i -> esPolynomial n i ^-^ lone (n + i)) indices
    gbasis  = groebner0 gPolys
    spray'  = spray ^-^ constantSpray (getConstantTerm spray)
    g       = sprayDivisionRemainder spray' gbasis
    gpowers = HM.keys g
    check1  = minimum (map nvariables gpowers) > n
    expnts  = map exponents gpowers
    check2  = DF.all (DF.all (0 ==)) (map (S.take n) expnts) 

-- | Whether a spray can be written as a polynomial of a given list of sprays
-- (the sprays in the list must belong to the same polynomial ring as the spray); 
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
isPolynomialOf spray sprays = result 
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
          constantTerm = constantSpray (getConstantTerm spray)
          spray'       = spray ^-^ constantTerm
          g            = sprayDivisionRemainder spray' gbasis0
          gpowers      = HM.keys g
          check1       = minimum (map nvariables gpowers) > n
          expnts       = map exponents gpowers
          check2       = DF.all (DF.all (0 ==)) (map (S.take n) expnts)
          checks       = check1 && check2
          poly         = if checks
            then Just $ dropXis g ^+^ constantTerm
            else Nothing
          dropXis = HM.mapKeys f
          f (Powers expnnts _) = Powers (S.drop n expnnts) n


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

-- | determinant of a matrix
detLaplace :: forall a. (Eq a, AlgRing.C a) => Matrix a -> a
detLaplace m = if nrows m == 1 
  then 
    m DM.! (1,1)
  else 
    suml1 [negateIf i (times (m DM.! (i,1)) (detLaplace (minorMatrix i 1 m))) 
           | i <- [1 .. nrows m]]
  where 
    suml1      = foldl1' (AlgAdd.+)
    negateIf i = if even i then AlgAdd.negate else id
    times :: a -> a -> a
    times x y = if x == AlgAdd.zero then AlgAdd.zero else x AlgRing.* y

-- | the coefficients of a spray as a univariate spray in x_1 with 
-- spray coefficients
sprayCoefficients :: (Eq a, AlgRing.C a) => Spray a -> [Spray a]
sprayCoefficients spray = 
  if n == 0 
    then [constantTerm]
    else reverse sprays
  where
    n = numberOfVariables spray 
    (powers, coeffs) = unzip (HM.toList spray)
    expnts           = map exponents powers
    constantTerm = 
      constantSpray $ fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) spray)
    (expnts', coeffs') = 
      unzip $ filter (\(s,_) -> S.length s > 0) (zip expnts coeffs)
    xpows              = map (`index` 0) expnts'
    expnts''           = map (S.deleteAt 0) expnts'
    powers''           = map (\s -> Powers s (S.length s)) expnts''
    sprays''           = zipWith (curry fromMonomial) powers'' coeffs'
    imap               = IM.fromListWith (^+^) (zip xpows sprays'')
    imap'              = IM.insertWith (^+^) 0 constantTerm imap
    permutation = [2 .. n] ++ [1]
    sprays = [
        permuteVariables permutation (fromMaybe AlgAdd.zero (IM.lookup i imap')) 
        | i <- [0 .. maximum xpows]
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
      map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys p))
    qexpnts = 
      map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys q))
    p0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) p)
    q0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) q)
    pcoeffs = if null pexpnts 
      then [p0]
      else [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) p) 
            | i <- [maxp, maxp-1 .. 1]] ++ [p0]
      where
        maxp = maximum pexpnts
    qcoeffs = if null qexpnts 
      then [q0]
      else [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) q) 
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
      map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys p))
    qexpnts = 
      map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys q))
    p0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) p)
    q0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) q)
    pcoeffs = if null pexpnts 
      then [p0]
      else [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) p) 
            | i <- [maxp, maxp-1 .. 1]] ++ [p0]
      where
        maxp = maximum pexpnts
    qcoeffs = if null qexpnts 
      then [q0]
      else [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) q) 
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
  | sprayA == zeroSpray || sprayB == zeroSpray 
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
      if snd division == zeroSpray 
        then fst division 
        else error "exactDivisionBy: should not happen."
      where
        division = sprayDivision a b
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
    (powers, coeffs) = unzip (HM.toList spray')
    expnts           = map exponents powers
    constantTerm = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) spray')
    (expnts', coeffs') = 
      unzip $ filter (\(s,_) -> (not . S.null) s) (zip expnts coeffs)
    xpows = map (`index` 0) expnts'
    expnts'' = map (S.deleteAt 0) expnts'
    powers'' = map (\s -> Powers s (S.length s)) expnts''
    sprays'' = zipWith (curry fromMonomial) powers'' coeffs'
    imap   = IM.fromListWith (^+^) (zip xpows sprays'')
    imap'  = IM.insertWith (^+^) 0 (constantSpray constantTerm) imap
    deg    = maximum xpows
    sprays = [
        fromMaybe AlgAdd.zero (IM.lookup i imap')
        | i <- [deg, deg-1 .. 0]
      ]

-- | the degree of a spray as a univariate spray in x_n with spray coefficients
degree :: (Eq a, AlgAdd.C a) => Int -> Spray a -> Int
degree n spray 
  | numberOfVariables spray == 0 = 
      if spray == zeroSpray 
        then minBound -- (should not happen)
        else 0
  | numberOfVariables spray /= n = 0
  | otherwise                    = maximum xpows
    where
      permutation = [2 .. n] ++ [1]
      spray'      = permuteVariables permutation spray
      expnts      = map exponents $ HM.keys spray'
      expnts'     = filter (not . S.null) expnts
      xpows       = map (`index` 0) expnts'

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
    (powers, coeffs) = unzip (HM.toList spray')
    expnts           = map exponents powers
    constantTerm = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) spray')
    (expnts', coeffs') = 
      unzip $ filter (\(s,_) -> not $ S.null s) (zip expnts coeffs)
    xpows = map (`index` 0) expnts'
    deg   = maximum xpows
    is    = elemIndices deg xpows
    expnts'' = [S.deleteAt 0 (expnts' !! i) | i <- is]
    powers'' = map (\s -> Powers s (S.length s)) expnts''
    coeffs'' = [coeffs' !! i | i <- is]
    leadingCoeff = 
      foldl1' (^+^) (zipWith (curry fromMonomial) powers'' coeffs'')

-- | pseudo-division of two sprays, assuming degA >= degB >= 0
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
      if degR < degB || sprayR == zeroSpray
        then (q ^*^ sprayQ, q ^*^ sprayR)
        else go (ellB ^*^ sprayR ^-^ sprayS ^*^ sprayB) 
                (ellB ^*^ sprayQ ^+^ sprayS) 
                (e - 1)
      where
        (degR, ellR) = degreeAndLeadingCoefficient n sprayR
        q            = ellB ^**^ e
        sprayXn      = lone n 
        sprayS       = ellR ^*^ sprayXn ^**^ (degR - degB)

-- | recursive GCD function
gcdKX1dotsXn :: forall a. (Eq a, AlgField.C a) 
                => Int -> Spray a -> Spray a -> Spray a
gcdKX1dotsXn n sprayA sprayB
  | n == 0              = constantSpray $ gcdKX0 sprayA sprayB
  | degB > degA         = gcdKX1dotsXn n sprayB sprayA 
  | sprayB == zeroSpray = sprayA
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
      if snd division == zeroSpray 
        then fst division 
        else error "exactDivisionBy: should not happen."
      where
        division = sprayDivision a b
    reduceSpray :: Spray a -> Spray a
    reduceSpray spray = exactDivisionBy cntnt spray 
      where
        coeffs = sprayCoefficients' n' spray
        cntnt  = foldl1' gcdKX1dotsXm coeffs
    contA   = content sprayA
    contB   = content sprayB
    d       = gcdKX1dotsXm contA contB 
    sprayA' = exactDivisionBy contA sprayA 
    sprayB' = exactDivisionBy contB sprayB 
    go :: Spray a -> Spray a -> Spray a -> Spray a -> Spray a
    go sprayA'' sprayB'' g h 
      | sprayR == zeroSpray           = d ^*^ reduceSpray sprayB''
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
