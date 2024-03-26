{-|
Module      : Math.Algebra.Hspray
Description : Multivariate polynomials on a ring.
Copyright   : (c) Stéphane Laurent, 2023
License     : GPL-3
Maintainer  : laurent_step@outlook.fr

Deals with multivariate polynomials on a commutative ring. See README for examples.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Algebra.Hspray
  ( 
  -- * Types
    Powers (..)
  , Spray
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
  , prettySprayXYZ
  -- * Queries on a spray
  , getCoefficient
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
  -- * Gröbner basis
  , groebner
  , reduceGroebnerBasis
  -- * Symmetric polynomials
  , esPolynomial
  , isSymmetricSpray
  -- * Resultant and subresultants
  , resultant
  , resultant1
  , subresultants
  , subresultants1
  -- * Miscellaneous
  , fromList
  , toList
  , fromRationalSpray
  , leadingTerm
  , isPolynomialOf
  , bombieriSpray
  , pseudoDivision
  ) where
import qualified Algebra.Additive              as AlgAdd
import qualified Algebra.Field                 as AlgField
import qualified Algebra.Module                as AlgMod
import qualified Algebra.Ring                  as AlgRing
import qualified Control.Arrow                 as Arrow
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
                                                , nub
                                                , foldl1'
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


infixr 3 &&&

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) = (Arrow.&&&)

infixr 7 *^, .^

infixl 6 ^+^, ^-^

infixl 7 ^*^

infixr 8 ^**^


data Powers = Powers
  { exponents  :: Seq Int
  , nvariables :: Int
  }
  deriving Show

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

instance Eq Powers where
  (==) :: Powers -> Powers -> Bool
  pows1 == pows2 = exponents pows1' == exponents pows2'
    where (pows1', pows2') = harmonize (pows1, pows2)

instance Hashable Powers where
  hashWithSalt :: Int -> Powers -> Int
  hashWithSalt k pows = hashWithSalt k (exponents pows, nvariables pows)

type Spray a = HashMap Powers a

type Monomial a = (Powers, a)

instance (AlgAdd.C a, Eq a) => AlgAdd.C (Spray a) where
  p + q = addSprays p q
  zero   = HM.empty
  negate = negateSpray

instance (AlgRing.C a, Eq a) => AlgMod.C a (Spray a) where
  lambda *> p = scaleSpray lambda p

instance (AlgRing.C a, Eq a) => AlgRing.C (Spray a) where
  p * q = multSprays p q
  one = lone 0

{- instance (AlgRing.C a, Eq a) => Num (Spray a) where
  p + q = addSprays p q
  negate = negateSpray
  p * q = multSprays p q
  fromInteger n = fromInteger n .^ AlgRing.one
  abs _ = error "Prelude.Num.abs: inappropriate abstraction"
  signum _ = error "Prelude.Num.signum: inappropriate abstraction"
 -} 

-- | Addition of two sprays
(^+^) :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
(^+^) p q = p AlgAdd.+ q

-- | Substraction of two sprays
(^-^) :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
(^-^) p q = p AlgAdd.- q

-- | Multiply two sprays
(^*^) :: (AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
(^*^) p q = p AlgRing.* q

-- | Power of a spray
(^**^) :: (AlgRing.C a, Eq a) => Spray a -> Int -> Spray a
(^**^) p n = AlgRing.product (replicate n p)

-- | Scale spray by a scalar
(*^) :: (AlgRing.C a, Eq a) => a -> Spray a -> Spray a
(*^) lambda pol = lambda AlgMod.*> pol

-- | Scale spray by an integer
--
-- prop> 3 .^ p == p ^+^ p ^+^ p
(.^) :: (AlgAdd.C a, Eq a) => Int -> Spray a -> Spray a
(.^) k pol = if k >= 0
  then AlgAdd.sum (replicate k pol)
  else AlgAdd.negate $ AlgAdd.sum (replicate (-k) pol)

-- | drop trailing zeros
simplifyPowers :: Powers -> Powers
simplifyPowers pows = Powers s (S.length s)
  where s = dropWhileR (== 0) (exponents pows)

-- | drop trailing zeros in the powers of a spray
simplifySpray :: Spray a -> Spray a
simplifySpray = HM.mapKeys simplifyPowers

-- | simplify powers and remove zero terms
cleanSpray :: (AlgAdd.C a, Eq a) => Spray a -> Spray a
cleanSpray p = HM.filter (/= AlgAdd.zero) (simplifySpray p)

-- | addition of two sprays
addSprays :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
addSprays p q = cleanSpray $ HM.foldlWithKey' f p q
  where f s powers coef = HM.insertWith (AlgAdd.+) powers coef s

-- | opposite spray
negateSpray :: AlgAdd.C a => Spray a -> Spray a
negateSpray = HM.map AlgAdd.negate

-- | scale a spray by a scalar
scaleSpray :: (AlgRing.C a, Eq a) => a -> Spray a -> Spray a
scaleSpray lambda p = cleanSpray $ HM.map (lambda AlgRing.*) p

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
  -> Spray a -- ^ the spray
  -> Spray a
derivSpray i p = if i >= 1 
  then cleanSpray $ HM.fromListWith (AlgAdd.+) monomials
  else error "derivSpray: invalid index."
 where
  p'        = HM.toList p
  monomials = [ derivMonomial i mp | mp <- p' ]

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
    powers = Powers expnts' (S.length expnts')

-- | number of variables in a spray
numberOfVariables :: Spray a -> Int
numberOfVariables spray = maximum (map nvariables powers)
  where
    powers = HM.keys spray

-- | evaluates a monomial
evalMonomial :: AlgRing.C a => [a] -> Monomial a -> a
evalMonomial xyz (powers, coeff) = 
  coeff AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows)
  where pows = DF.toList (fromIntegral <$> exponents powers)

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
    pows = exponents powers
    n = nvariables powers
    indices = findIndices isJust (take n subs)
    pows' = [fromIntegral (pows `index` i) | i <- indices]
    xyz = [fromJust (subs !! i) | i <- indices]
    coeff' = coeff AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows')
    f i a = if i `elem` indices then 0 else a
    pows'' = S.mapWithIndex f pows
    powers'' = simplifyPowers $ Powers pows'' n

-- | Substitutes some variables in a spray
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
    n = numberOfVariables spray
    monomials = HM.toList spray
    spray' = foldl1 (^+^) (map (fromMonomial . substituteMonomial subs) monomials)

-- | Converts a spray with rational coefficients to a spray with double coefficients
-- (useful for evaluation)
fromRationalSpray :: Spray Rational -> Spray Double
fromRationalSpray = HM.map fromRational

-- | Composes a spray with a change of variables
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = x ^+^ y
-- >>> q = composeSpray p [z, x ^+^ y ^+^ z]
-- >>> putStrLn $ prettySprayXYZ q
-- (1) X + (1) Y + (2) Z
composeSpray :: (AlgRing.C a, Eq a) => Spray a -> [Spray a] -> Spray a
composeSpray p = evalSpray (identify p)
  where 
    ---- identify :: (AlgRing.C a, Eq a) => Spray a -> Spray (Spray a)
    identify = HM.map constantSpray

-- | Creates a spray from list of terms
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
-- prop> permuteVariables p [3, 1, 2] == f x3 x1 x2
permuteVariables :: Spray a -> [Int] -> Spray a
permuteVariables spray permutation = 
  if n' >= n && isPermutation permutation  
    then spray'
    else error "permuteVariables: invalid permutation."
  where
    n = numberOfVariables spray
    n' = maximum permutation
    isPermutation pmtn = minimum pmtn == 1 && length (nub pmtn) == n'
    intmap = IM.fromList (zip permutation [1 .. n'])
    invpermutation = [intmap IM.! i | i <- [1 .. n']]
    permuteSeq x = S.mapWithIndex (\i _ -> x `index` (invpermutation !! i - 1)) x 
    (powers, coeffs) = unzip (HM.toList spray)
    expnts = map exponents powers
    expnts' = map (permuteSeq . growSequence' n') expnts
    powers' = map (\exps -> simplifyPowers (Powers exps n')) expnts'
    spray' = HM.fromList (zip powers' coeffs)

-- | Swaps two variables of a spray
-- 
-- prop> swapVariables (1, 3) p == permuteVariables p [3, 2, 1]
swapVariables :: Spray a -> (Int, Int) -> Spray a
swapVariables spray (i, j) = 
  if i>=1 && j>=1  
    then spray'
    else error "swapVariables: invalid indices."
  where
    n = maximum [numberOfVariables spray, i, j]
    f k | k == i    = j
        | k == j    = i
        | otherwise = k
    transposition = map f [1 .. n]
    permuteSeq x = S.mapWithIndex (\ii _ -> x `index` (transposition !! ii - 1)) x 
    (powers, coeffs) = unzip (HM.toList spray)
    expnts = map exponents powers
    expnts' = map (permuteSeq . growSequence' n) expnts
    powers' = map (\exps -> simplifyPowers (Powers exps n)) expnts'
    spray' = HM.fromList (zip powers' coeffs)


-- pretty stuff ---------------------------------------------------------------

-- | prettyPowers "x" [0, 2, 1] = x^(0, 2, 1)
prettyPowers :: String -> [Int] -> Text
prettyPowers var pows = append (pack x) (cons '(' $ snoc string ')')
 where
  x      = " " ++ var ++ "^"
  string = intercalate (pack ", ") (map (pack . show) pows)

-- | Pretty form of a spray
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray show "x" p
-- (2) * x^(1) + (3) * x^(0, 2) + (-4) * x^(0, 0, 3)
prettySpray 
  :: (a -> String) -- ^ function mapping a coefficient to a string, typically 'show'
  -> String        -- ^ a string denoting the variable, e.g. \"x\"
  -> Spray a       -- ^ the spray
  -> String
prettySpray prettyCoef var p = unpack $ intercalate (pack " + ") stringTerms
 where
  stringTerms = map stringTerm (sortBy (flip compare `on` fexpts) (HM.toList p))
  fexpts term = exponents $ fst term
  stringTerm term = append
    (snoc (snoc (cons '(' $ snoc stringCoef ')') ' ') '*')
    (prettyPowers var pows)
   where
    pows       = DF.toList $ exponents (fst term)
    stringCoef = pack $ prettyCoef (snd term)

-- | prettyPowers' [0, 2, 1] = "x2^2x3"
prettyPowers' :: Seq Int -> Text
prettyPowers' pows = pack x1x2x3
 where
  n = S.length pows
  f i p 
    | p == 0 = ""
    | p == 1 = "x" ++ show i
    | otherwise = "x" ++ show i ++ "^" ++ show p
  x1x2x3 = concatMap (\i -> f i (pows `index` (i-1))) [1 .. n]

-- | Pretty form of a spray, with monomials showed as "x1x3^2"
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySpray' p
-- (2) x1 + (3) x2^2 + (-4) x3^3 
prettySpray' :: (Show a) => Spray a -> String
prettySpray' spray = unpack $ intercalate (pack " + ") terms
 where
  terms = map stringTerm (sortBy (flip compare `on` fexpts) (HM.toList spray))
  fexpts term = exponents $ fst term
  stringTerm term = append stringCoef'' (prettyPowers' pows)
   where
    pows       = exponents (fst term)
    constant   = S.length pows == 0
    stringCoef = pack $ show (snd term)
    stringCoef' = cons '(' $ snoc stringCoef ')'
    stringCoef'' = if constant then stringCoef' else snoc stringCoef' ' '

-- | prettyPowersXYZ [1, 2, 1] = XY^2Z
prettyPowersXYZ :: Seq Int -> Text
prettyPowersXYZ pows = if n <= 3 
  then pack xyz
  else error "there is more than three variables"
 where
  n = S.length pows
  gpows = growSequence pows n 3
  f letter p 
    | p == 0 = ""
    | p == 1 = letter
    | otherwise = letter ++ "^" ++ show p
  x = f "X" (gpows `index` 0)
  y = f "Y" (gpows `index` 1)
  z = f "Z" (gpows `index` 2)
  xyz = x ++ y ++ z

-- | Pretty form of a spray having at more three variables
--
-- >>> x :: lone 1 :: Spray Int
-- >>> y :: lone 2 :: Spray Int
-- >>> z :: lone 3 :: Spray Int
-- >>> p = 2*^x ^+^ 3*^y^**^2 ^-^ 4*^z^**^3
-- >>> putStrLn $ prettySprayXYZ p
-- (2) X + (3) Y^2 + (-4) Z^3
prettySprayXYZ :: (Show a) => Spray a -> String
prettySprayXYZ spray = unpack $ intercalate (pack " + ") terms
 where
  terms = map stringTerm (sortBy (flip compare `on` fexpts) (HM.toList spray))
  fexpts term = exponents $ fst term
  stringTerm term = append stringCoef'' (prettyPowersXYZ pows)
   where
    pows         = exponents (fst term)
    constant     = S.length pows == 0
    stringCoef   = pack $ show (snd term)
    stringCoef'  = cons '(' $ snoc stringCoef ')'
    stringCoef'' = if constant then stringCoef' else snoc stringCoef' ' '


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


-- division stuff -------------------------------------------------------------

-- | index of the maximum of a list
maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

-- | Leading term of a spray 
leadingTerm :: Spray a -> Monomial a
leadingTerm p = (biggest, p HM.! biggest) 
  where
    powers = HM.keys p
    i = maxIndex $ map exponents powers
    biggest = powers !! i

-- | whether a monomial divides another monomial
divides :: Monomial a -> Monomial a -> Bool
divides (powsP, _) (powsQ, _) = S.length expntsP <= S.length expntsQ && lower
  where
    expntsP = exponents powsP
    expntsQ = exponents powsQ
    lower = DF.all (\(x, y) -> x <= y) (S.zip expntsP expntsQ)

-- | quotient of monomial Q by monomial p, assuming P divides Q
quotient :: AlgField.C a => Monomial a -> Monomial a -> Monomial a
quotient (powsQ, coeffQ) (powsP, coeffP) = (pows, coeff)
  where
    (powsP', powsQ') = harmonize (powsP, powsQ)
    expntsP = exponents powsP'
    expntsQ = exponents powsQ'
    expnts = S.zipWith (-) expntsQ expntsP
    n = nvariables powsP'
    pows = Powers expnts n
    coeff = coeffQ AlgField./ coeffP

-- | Remainder of the division of a spray by a list of divisors, 
-- using the lexicographic ordering of the monomials
sprayDivision :: forall a. (Eq a, AlgField.C a) => Spray a -> [Spray a] -> Spray a
sprayDivision p qs = 
  if n == 0 
    then error "sprayDivision: the list of divisors is empty." 
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
      | i == n = g lts s r 
      | otherwise = go lts news r (i+1) newdivoccured
        where
          (q, ltq) = qsltqs !! i
          newdivoccured = divides ltq lts
          news = if newdivoccured
            then s ^-^ (fromMonomial (quotient lts ltq) ^*^ q)
            else s
    ogo :: Spray a -> Spray a -> (Spray a, Spray a)
    ogo !s !r 
      | s == AlgAdd.zero = (s, r)
      | otherwise = ogo s' r'
        where
          (s', r') = go (leadingTerm s) s r 0 False


-- Groebner stuff -------------------------------------------------------------

-- | slight modification of `sprayDivision` to speed up groebner00
sprayDivision' :: forall a. (Eq a, AlgField.C a) => Spray a -> HashMap Int (Spray a, Monomial a) -> Spray a
sprayDivision' p qsltqs = snd $ ogo p AlgAdd.zero
  where
    n = HM.size qsltqs
    g :: Monomial a -> Spray a -> Spray a -> (Spray a, Spray a)
    g lts s r = (s ^-^ ltsspray, r ^+^ ltsspray)
      where
        ltsspray = fromMonomial lts 
    go :: Monomial a -> Spray a -> Spray a -> Int -> Bool -> (Spray a, Spray a)
    go lts !s r !i !divoccured
      | divoccured = (s, r)
      | i == n = g lts s r 
      | otherwise = go lts news r (i+1) newdivoccured
        where
          (q, ltq) = qsltqs HM.! i
          newdivoccured = divides ltq lts
          news = if newdivoccured
            then s ^-^ (fromMonomial (quotient lts ltq) ^*^ q)
            else s
    ogo :: Spray a -> Spray a -> (Spray a, Spray a)
    ogo !s !r 
      | s == AlgAdd.zero = (s, r)
      | otherwise = ogo s' r'
        where
          (s', r') = go (leadingTerm s) s r 0 False

combn2 :: Int -> Int -> HashMap Int (Int, Int)
combn2 n s = HM.fromList (zip [0 .. n-2] (zip row1 row2)) 
  where
    row1 = drop s $ concatMap (\i -> [0 .. (i-1)]) [1 .. n-1]
    row2 = drop s $ concatMap (\i -> replicate i i) [1 .. n-1]

sPolynomial :: (Eq a, AlgField.C a) => (Spray a, Monomial a) -> (Spray a, Monomial a) -> Spray a
sPolynomial pltp qltq = wp ^*^ p ^-^ wq ^*^ q
  where
    p = fst pltp
    q = fst qltq
    (lpowsP, lcoefP) = snd pltp
    (lpowsQ, lcoefQ) = snd qltq
    (lpowsP', lpowsQ') = harmonize (lpowsP, lpowsQ)
    lexpntsP = exponents lpowsP'
    lexpntsQ = exponents lpowsQ'
    gamma = S.zipWith max lexpntsP lexpntsQ
    betaP = S.zipWith (-) gamma lexpntsP
    betaQ = S.zipWith (-) gamma lexpntsQ
    n = nvariables lpowsP'
    wp = fromMonomial (Powers betaP n, AlgField.recip lcoefP)
    wq = fromMonomial (Powers betaQ n, AlgField.recip lcoefQ)

-- | groebner basis, not minimal and not reduced
groebner00 :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
groebner00 sprays = go 0 j0 combins0 spraysMap
  where
    j0 = length sprays
    combins0 = combn2 j0 0
    ltsprays = map leadingTerm sprays
    spraysltsprays = zip sprays ltsprays 
    spraysMap = HM.fromList (zip [0 .. j0-1] spraysltsprays)
    go :: Int -> Int -> HashMap Int (Int, Int) -> HashMap Int (Spray a, Monomial a) -> [Spray a]
    go !i !j !combins !gpolysMap
      | i == length combins = map fst (HM.elems gpolysMap)
      | otherwise = go i' j' combins' gpolysMap'
        where
          (k, l) = combins HM.! i
          sfg = sPolynomial (gpolysMap HM.! k) (gpolysMap HM.! l)
          sbarfg = sprayDivision' sfg gpolysMap
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
    n = length basis00
    basis00 = groebner00 sprays
    go :: Int -> [Int] -> [Int]
    go !i toRemove
      | i == n = toRemove
      | otherwise = go (i+1) toRemove'
        where
          ltf = leadingTerm (basis00 !! i)
          toDrop = toRemove ++ [i]
          igo :: Int -> Bool
          igo !j 
            | j == n = False
            | j `elem` toDrop = igo (j+1)
            | otherwise = ok || igo (j+1)
              where 
                ok = divides (leadingTerm (basis00 !! j)) ltf
          toRemove' = if igo 0 then toDrop else toRemove
    discard = go 0 []

-- | Reduces a Groebner basis
reduceGroebnerBasis :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
reduceGroebnerBasis gbasis = 
  if length gbasis >= 2 then map reduction [0 .. n-1] else ngbasis
  where
    normalize :: Spray a -> Spray a
    normalize spray = AlgField.recip coef *^ spray
      where
        (_, coef) = leadingTerm spray
    ngbasis = map normalize gbasis
    n = length ngbasis
    reduction :: Int -> Spray a
    reduction i = sprayDivision (ngbasis !! i) rest
      where
        rest = [ngbasis !! k | k <- [0 .. n-1] \\ [i]]

-- | Groebner basis (always minimal and possibly reduced)
--
-- prop> groebner ps True = reduceGroebnerBasis (groebner ps False)
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
        f []     = error "should not happen"
        f (b:bs) = (b, bs)
        (q, qs) = f (take (n-i+1) ys)
        dc      = product [(n-k+1) .. (n-1)] `div` product [1 .. i-1]

-- | generate all permutations of a binary sequence
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
  | k <= 0 || n <= 0 = error "esPolynomial: both arguments must be positive integers."
  | k > n = AlgAdd.zero
  | otherwise = simplifySpray spray
  where
    perms = permutationsBinarySequence (n-k) k
    spray = HM.fromList $ map (\expts -> (Powers expts n, AlgRing.one)) perms

-- | Whether a spray is a symmetric polynomial
isSymmetricSpray :: forall a. (AlgField.C a, Eq a) => Spray a -> Bool
isSymmetricSpray spray = check1 && check2 
  where
    n = numberOfVariables spray
    indices = [1 .. n]
    esPolys = map (\i -> esPolynomial n i :: Spray a) indices
    yPolys = map (\i -> lone (n + i) :: Spray a) indices
    gPolys = zipWith (^-^) esPolys yPolys
    gbasis = groebner0 gPolys
    g = sprayDivision spray gbasis
    gpowers = HM.keys g
    check1 = minimum (map nvariables gpowers) > n
    expnts = map exponents gpowers
    check2 = DF.all (DF.all (0 ==)) (map (S.take n) expnts) 

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
isPolynomialOf :: forall a. (AlgField.C a, Eq a) => Spray a -> [Spray a] -> (Bool, Maybe (Spray a))
isPolynomialOf spray sprays = result 
  where
    n = numberOfVariables spray
    n' = maximum $ map numberOfVariables sprays
    result
      | n > n' = (False, Nothing)
      | n < n' = error "not enough variables in the spray" 
      | otherwise = (checks, poly)
        where
          m = length sprays
          yPolys = map (\i -> lone (n + i) :: Spray a) [1 .. m]
          gPolys = zipWith (^-^) sprays yPolys
          gbasis0 = groebner0 gPolys
          g = sprayDivision spray gbasis0
          gpowers = HM.keys g
          check1 = minimum (map nvariables gpowers) > n
          expnts = map exponents gpowers
          check2 = DF.all (DF.all (0 ==)) (map (S.take n) expnts)
          checks = check1 && check2
          poly = if checks
            then Just $ dropXis g
            else Nothing
          dropXis = HM.mapKeys f
          f (Powers expnnts _) = Powers (S.drop n expnnts) n


-- resultant ------------------------------------------------------------------

-- sylvester matrix
sylvesterMatrix :: AlgAdd.C a => [a] -> [a] -> Matrix a
sylvesterMatrix x y = fromLists (xrows ++ yrows) 
  where
    m = length x - 1
    n = length y - 1
    xrows = [replicate i AlgAdd.zero ++ x ++ replicate (n-i-1) AlgAdd.zero | i <- [0 .. n-1]]
    yrows = [replicate i AlgAdd.zero ++ y ++ replicate (m-i-1) AlgAdd.zero | i <- [0 .. m-1]]

-- "truncated" Sylvester matrix
sylvesterMatrix' :: AlgRing.C a => [a] -> [a] -> Int -> Matrix a
sylvesterMatrix' x y k = if s == 0 
  then fromLists [[AlgRing.one]] -- plays the role of the empty matrix: determinant=1 (because the empty matrix is not allowed)
  else submatrix 1 s 1 s $ fromLists (xrows ++ yrows) 
  where
    m = length x - 1
    n = length y - 1
    s = m + n - 2*k
    xrows = [replicate i AlgAdd.zero ++ x ++ replicate (n-i-1) AlgAdd.zero | i <- [0 .. n-1-k]]
    yrows = [replicate i AlgAdd.zero ++ y ++ replicate (m-i-1) AlgAdd.zero | i <- [0 .. m-1-k]]

-- determinant
detLaplace :: forall a. (Eq a, AlgRing.C a) => Matrix a -> a
detLaplace m = if nrows m == 1 
  then m DM.! (1,1)
  else suml1 [negateIf i (times (m DM.! (i,1)) (detLaplace (minorMatrix i 1 m))) | i <- [1 .. nrows m]]
  where 
    suml1 = foldl1' (AlgAdd.+)
    negateIf i = if even i then AlgAdd.negate else id
    times :: a -> a -> a
    times x y = if x == AlgAdd.zero then AlgAdd.zero else x AlgRing.* y

-- the coefficients of a spray as a univariate spray in x with spray coefficients
sprayCoefficients :: (Eq a, AlgRing.C a) => Spray a -> [Spray a]
sprayCoefficients spray = reverse sprays
  where
    (powers, coeffs) = unzip (HM.toList spray)
    expnts = map exponents powers
    constantTerm = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) spray)
    (expnts', coeffs') = unzip $ filter (\(s,_) -> S.length s > 0) (zip expnts coeffs)
    xpows = map (`index` 0) expnts'
    expnts'' = map (S.deleteAt 0) expnts'
    powers'' = map (\s -> Powers s (S.length s)) expnts''
    sprays'' = zipWith (curry fromMonomial) powers'' coeffs'
    imap = IM.fromListWith (^+^) (zip xpows sprays'')
    imap' = IM.insertWith (^+^) 0 (constantSpray constantTerm) imap
    sprays = [fromMaybe AlgAdd.zero (IM.lookup i imap') | i <- [0 .. maximum xpows]]

pseudoDivision :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> (Spray a, Spray a)
pseudoDivision sprayA sprayB = go sprayA zeroSpray (degA - degB + 1)
  where
    degA = length (sprayCoefficients sprayA) - 1 -- degree(A)
    degEll spray = ((\x -> length x - 1) &&& (!! 0)) (sprayCoefficients spray) -- (degree, ell) 
    (degB, ellB) = degEll sprayB
    go sprayR sprayQ e = 
      if degR < degB
        then (q ^*^ sprayQ, q ^*^ sprayR)
        else (ellB ^*^ sprayQ ^+^ sprayS, ellB ^*^ sprayR ^-^ sprayS ^*^ sprayB)
      where
        (degR, ellR) = degEll sprayR
        q = ellB ^**^ e
        sprayS = ellR ^*^ (lone 1)^**^(degR - degB)



-- | Resultant of two univariate sprays
resultant1 :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> a
resultant1 p q = detLaplace $ sylvesterMatrix pcoeffs qcoeffs
  where
    pexpnts = map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys p))
    qexpnts = map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys q))
    p0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) p)
    q0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) q)
    pcoeffs = reverse $ if null pexpnts 
      then [p0]
      else p0 : [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) p) | i <- [1 .. maximum pexpnts]]
    qcoeffs = reverse $ if null qexpnts 
      then [q0]
      else q0 : [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) q) | i <- [1 .. maximum qexpnts]]

-- | Subresultants of two univariate sprays
subresultants1 :: (Eq a, AlgRing.C a) => Spray a -> Spray a -> [a]
subresultants1 p q = map (detLaplace . sylvesterMatrix' pcoeffs qcoeffs) [0 .. min d e - 1]
  where
    pexpnts = map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys p))
    qexpnts = map (`index` 0) $ filter (not . S.null) (map exponents (HM.keys q))
    p0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) p)
    q0 = fromMaybe AlgAdd.zero (HM.lookup (Powers S.empty 0) q)
    pcoeffs = reverse $ if null pexpnts 
      then [p0]
      else p0 : [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) p) | i <- [1 .. maximum pexpnts]]
    qcoeffs = reverse $ if null qexpnts 
      then [q0]
      else q0 : [fromMaybe AlgAdd.zero (HM.lookup (Powers (S.singleton i) 1) q) | i <- [1 .. maximum qexpnts]]
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
    then detLaplace $ sylvesterMatrix (sprayCoefficients p') (sprayCoefficients q')
    else error "resultant: invalid variable index."
  where
    n = max (numberOfVariables p) (numberOfVariables q)
    permutation = var : [1 .. var-1] ++ [var+1 .. n]
    p' = permuteVariables p permutation
    q' = permuteVariables q permutation

-- | Subresultants of two sprays
subresultants :: (Eq a, AlgRing.C a) 
  => Int     -- ^ indicator of the variable with respect to which the resultant is desired (e.g. 1 for x)
  -> Spray a 
  -> Spray a 
  -> [Spray a]
subresultants var p q 
  | var < 1 = error "subresultants: invalid variable index."
  | var > n = error "subresultants: too large variable index."
  | otherwise = map (detLaplace . sylvesterMatrix' pcoeffs qcoeffs) [0 .. min d e - 1]
  where
    pcoeffs = sprayCoefficients p'
    qcoeffs = sprayCoefficients q'
    d = length pcoeffs
    e = length qcoeffs
    n = max (numberOfVariables p) (numberOfVariables q)
    permutation = var : [1 .. var-1] ++ [var+1 .. n]
    p' = permuteVariables p permutation
    q' = permuteVariables q permutation
