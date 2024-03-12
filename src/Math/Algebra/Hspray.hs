{-|
Module      : Math.Algebra.Hspray
Description : Multivariate polynomials on a ring.
Copyright   : (c) Stéphane Laurent, 2023
License     : GPL-3
Maintainer  : laurent_step@outlook.fr

Deals with multivariate polynomials on a ring. See README for examples.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Algebra.Hspray
  ( Powers (..)
  , Spray
  , Monomial
  , lone
  , unitSpray
  , constantSpray
  , fromList
  , toList
  , sprayTerms
  , (*^)
  , (.^)
  , (^+^)
  , (^-^)
  , (^*^)
  , (^**^)
  , evalSpray
  , fromRationalSpray
  , prettySpray
  , prettySpray'
  , prettySprayXYZ
  , composeSpray
  , bombieriSpray
  , derivSpray
  , leadingTerm
  , sprayDivision
  , groebner
  , esPolynomial
  , isSymmetricSpray
  , groebner00
  , groebner0
  ) where
import qualified Algebra.Additive              as AlgAdd
import qualified Algebra.Field                 as AlgField
import qualified Algebra.Module                as AlgMod
import qualified Algebra.Ring                  as AlgRing
import qualified Data.Foldable                 as DF
import           Data.Function                  ( on )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.Hashable                  ( Hashable(hashWithSalt) )
import           Data.List                      ( sortBy
                                                , maximumBy 
                                                , (\\)
                                                )
import           Data.Ord                       ( comparing )
import qualified Data.Sequence                 as S
import           Data.Sequence                  ( (><)
                                                , Seq (Empty, (:<|))
                                                , dropWhileR
                                                , (|>)
                                                , (<|)
                                                , index
                                                , adjust
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , cons
                                                , intercalate
                                                , pack
                                                , snoc
                                                , unpack
                                                )


infixr 7 *^, .^

infixl 6 ^+^, ^-^

infixl 7 ^*^

infixr 8 ^**^


data Powers = Powers
  { exponents  :: Seq Int
  , nvariables :: Int
  }
  deriving Show

growSequence :: Seq Int -> Int -> Int -> Seq Int
growSequence s m n = s >< t where t = S.replicate (n - m) 0

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
(.^) :: (AlgAdd.C a, Eq a) => Int -> Spray a -> Spray a
(.^) k pol = if k >= 0
  then AlgAdd.sum (replicate k pol)
  else AlgAdd.negate $ AlgAdd.sum (replicate (-k) pol)

simplifyPowers :: Powers -> Powers
simplifyPowers pows = Powers s (S.length s)
  where s = dropWhileR (== 0) (exponents pows)

simplifySpray :: Spray a -> Spray a
simplifySpray = HM.mapKeys simplifyPowers

cleanSpray :: (AlgAdd.C a, Eq a) => Spray a -> Spray a
cleanSpray p = HM.filter (/= AlgAdd.zero) (simplifySpray p)

addSprays :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
addSprays p q = cleanSpray $ HM.foldlWithKey' f p q
  where f s powers coef = HM.insertWith (AlgAdd.+) powers coef s

negateSpray :: AlgAdd.C a => Spray a -> Spray a
negateSpray = HM.map AlgAdd.negate

scaleSpray :: (AlgRing.C a, Eq a) => a -> Spray a -> Spray a
scaleSpray lambda p = cleanSpray $ HM.map (lambda AlgRing.*) p

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

derivSpray :: (AlgRing.C a, Eq a) => Int -> Spray a -> Spray a
derivSpray i p = cleanSpray $ HM.fromListWith (AlgAdd.+) monomials
 where
  p'        = HM.toList p
  monomials = [ derivMonomial i mp | mp <- p' ]


multMonomial :: AlgRing.C a => Monomial a -> Monomial a -> Monomial a
multMonomial (pows1, coef1) (pows2, coef2) = (pows, coef1 AlgRing.* coef2)
 where
  (pows1', pows2') = harmonize (pows1, pows2)
  expts            = S.zipWith (+) (exponents pows1') (exponents pows2')
  pows             = Powers expts (nvariables pows1')

multSprays :: (AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
multSprays p q = cleanSpray $ HM.fromListWith (AlgAdd.+) prods
 where
  p'    = HM.toList p
  q'    = HM.toList q
  prods = [ multMonomial mp mq | mp <- p', mq <- q' ]

-- | Spray corresponding to polynomial x_n
lone :: AlgRing.C a => Int -> Spray a
lone n = HM.singleton pows AlgRing.one
 where
  pows = if n == 0
    then Powers S.empty 0
    else Powers (S.replicate (n - 1) AlgAdd.zero |> AlgRing.one) n

-- | Unit spray
unitSpray :: AlgRing.C a => Spray a
unitSpray = lone 0

-- | Constant spray
constantSpray :: (AlgRing.C a, Eq a) => a -> Spray a
constantSpray c = c *^ lone 0

-- | evaluates a monomial
evalMonomial :: AlgRing.C a => [a] -> Monomial a -> a
evalMonomial xyz (powers, coeff) = coeff
  AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows)
  where pows = DF.toList (fromIntegral <$> exponents powers)

-- | Evaluate a spray
evalSpray :: AlgRing.C a => Spray a -> [a] -> a
evalSpray p xyz = AlgAdd.sum $ map (evalMonomial xyz) (HM.toList p)

-- | Converts a spray with rational coefficients to a spray with double coefficients
fromRationalSpray :: Spray Rational -> Spray Double
fromRationalSpray = HM.map fromRational

-- |
identify :: (AlgRing.C a, Eq a) => Spray a -> Spray (Spray a)
identify = HM.map constantSpray

-- | Compose a spray with a change of variables
composeSpray :: (AlgRing.C a, Eq a) => Spray a -> [Spray a] -> Spray a
composeSpray p = evalSpray (identify p)

-- | Create a spray from list of terms
fromList :: (AlgRing.C a, Eq a) => [([Int], a)] -> Spray a
fromList x = cleanSpray $ HM.fromList $ map
  (\(expts, coef) -> (Powers (S.fromList expts) (length expts), coef)) x

-- | pretty stuff
prettyPowers :: String -> [Int] -> Text
prettyPowers var pows = append (pack x) (cons '(' $ snoc string ')')
 where
  x      = " " ++ var ++ "^"
  string = intercalate (pack ", ") (map (pack . show) pows)

-- | Pretty form of a spray
prettySpray :: (a -> String) -> String -> Spray a -> String
prettySpray prettyCoef var p = unpack $ intercalate (pack " + ") stringTerms
 where
  stringTerms = map stringTerm (sortBy (compare `on` fexpts) (HM.toList p))
  fexpts term = exponents $ fst term
  stringTerm term = append
    (snoc (snoc (cons '(' $ snoc stringCoef ')') ' ') '*')
    (prettyPowers var pows)
   where
    pows       = DF.toList $ exponents (fst term)
    stringCoef = pack $ prettyCoef (snd term)

prettyPowers' :: Seq Int -> Text
prettyPowers' pows = pack x1x2x3
 where
  n = S.length pows
  f i p 
    | p == 0 = ""
    | p == 1 = "x" ++ show i
    | otherwise = "x" ++ show i ++ "^" ++ show p
  x1x2x3 = concatMap (\i -> f i (pows `index` (i-1))) [1 .. n]

-- | Pretty form of a spray
prettySpray' :: (Show a) => Spray a -> String
prettySpray' spray = unpack $ intercalate (pack " + ") terms
 where
  terms = map stringTerm (sortBy (compare `on` fexpts) (HM.toList spray))
  fexpts term = exponents $ fst term
  stringTerm term = append stringCoef'' (prettyPowers' pows)
   where
    pows       = exponents (fst term)
    constant   = S.length pows == 0
    stringCoef = pack $ show (snd term)
    stringCoef' = cons '(' $ snoc stringCoef ')'
    stringCoef'' = if constant then stringCoef' else snoc stringCoef' ' '

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

-- | Pretty form of a spray with at more three variables
prettySprayXYZ :: (Show a) => Spray a -> String
prettySprayXYZ spray = unpack $ intercalate (pack " + ") terms
 where
  terms = map stringTerm (sortBy (compare `on` fexpts) (HM.toList spray))
  fexpts term = exponents $ fst term
  stringTerm term = append stringCoef'' (prettyPowersXYZ pows)
   where
    pows         = exponents (fst term)
    constant     = S.length pows == 0
    stringCoef   = pack $ show (snd term)
    stringCoef'  = cons '(' $ snoc stringCoef ')'
    stringCoef'' = if constant then stringCoef' else snoc stringCoef' ' '

-- | Terms of a spray
sprayTerms :: Spray a -> HashMap (Seq Int) a
sprayTerms = HM.mapKeys exponents

-- | Spray as list
toList :: Spray a -> [([Int], a)]
toList p = HM.toList $ HM.mapKeys (DF.toList . exponents) p

-- | Bombieri spray
bombieriSpray :: AlgAdd.C a => Spray a -> Spray a
bombieriSpray = HM.mapWithKey f
 where
  f pows          = times (pfactorial $ exponents pows)
  pfactorial pows = product $ DF.toList $ factorial <$> S.filter (/= 0) pows
  factorial n     = product [1 .. n]
  times k x       = AlgAdd.sum (replicate k x)

-- division stuff -------------------------------------------------------------
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

-- | spray from monomial
fromMonomial :: Monomial a -> Spray a
fromMonomial (pows, coeff) = HM.singleton pows coeff

-- | Remainder of the division of a spray by a list of divisors, using the lexicographic ordering of the monomials
sprayDivision :: forall a. (Eq a, AlgField.C a) => Spray a -> [Spray a] -> Spray a
sprayDivision p qs = snd $ ogo p AlgAdd.zero
  where
    n = length qs
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
          q = qs !! i
          ltq = leadingTerm q
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
combn2 :: Int -> [(Int, Int)]
combn2 n = zip row1 row2 
  where
    row1 = concatMap (\i -> replicate (n-i) (i-1)) [1 .. n-1]
    row2 = concatMap (\i -> drop i [0 .. n-1]) [1 .. n-1]

sPolynomial :: (Eq a, AlgField.C a) => Spray a -> Spray a -> Spray a
sPolynomial p q = wp ^*^ p ^-^ wq ^*^ q
  where
    (lpowsP, lcoefP) = leadingTerm p
    (lpowsQ, lcoefQ) = leadingTerm q
    (lpowsP', lpowsQ') = harmonize (lpowsP, lpowsQ)
    lexpntsP = exponents lpowsP'
    lexpntsQ = exponents lpowsQ'
    gamma = S.zipWith max lexpntsP lexpntsQ
    betaP = S.zipWith (-) gamma lexpntsP
    betaQ = S.zipWith (-) gamma lexpntsQ
    n = nvariables lpowsP'
    wp = fromMonomial (Powers betaP n, AlgField.recip lcoefP)
    wq = fromMonomial (Powers betaQ n, AlgField.recip lcoefQ)

-- | Groebner basis, not minimal and not reduced
groebner00 :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
groebner00 sprays = go 0 j0 combins0 sprays HM.empty
  where
    j0 = length sprays
    combins0 = combn2 j0
    go :: Int -> Int -> [(Int, Int)] -> [Spray a] -> HashMap (Int, Int) (Spray a) -> [Spray a]
    go !i !j !combins !gpolys !spolys
      | i == length combins = gpolys
      | otherwise = go i' j' combins' gpolys' spolys'
        where
          combin@(k, l) = combins !! i
          sfg = sPolynomial (gpolys !! k) (gpolys !! l)
          ssnew = HM.singleton combin sfg
          spolys' = HM.union ssnew spolys
          sbarfg = sprayDivision sfg gpolys
          (i', j', gpolys', combins') = if sbarfg == AlgAdd.zero
            then
              (i+1, j, gpolys, combins)
            else
              (0, j+1, gpolys ++ [sbarfg], combn2 (j+1) \\ HM.keys spolys')

-- | Groebner basis, minimal but not reduced
groebner0 :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
groebner0 sprays = [normalize $ basis00 !! k | k <- [0 .. n-1] \\ discard]
  where
    basis00 = groebner00 sprays
    n = length basis00
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
    normalize :: Spray a -> Spray a
    normalize spray = AlgField.recip coef *^ spray
      where
        (_, coef) = leadingTerm spray

-- | Groebner basis (minimal and reduced)
groebner :: forall a. (Eq a, AlgField.C a) => [Spray a] -> [Spray a]
groebner sprays = map reduction [0 .. n-1]
  where
    basis0 = groebner0 sprays
    n = length basis0
    reduction :: Int -> Spray a
    reduction i = sprayDivision (basis0 !! i) rest
      where
        rest = [basis0 !! k | k <- [0 .. n-1] \\ [i]]

-- elementary symmetric polynomials -------------------------------------------

-- | Generates all permutations of a binary sequence
permutationsBinarySequences :: Int -> Int -> [Seq Int] 
permutationsBinarySequences nzeros nones = unfold1 next z 
  where
    z = (><) (S.replicate nzeros False) (S.replicate nones True)
    unfold1 :: (Seq Bool -> Maybe (Seq Bool)) -> Seq Bool -> [Seq Int]
    unfold1 f x = case f x of 
      Nothing -> [x'] 
      Just y  -> x' : unfold1 f y 
      where
        x' = fmap fromEnum x
    next :: Seq Bool -> Maybe (Seq Bool)
    next xs = case findj (S.reverse xs, S.empty) of 
      Nothing -> Nothing
      Just ( l:<|ls , rs) -> Just $ inc l ls (S.reverse rs, S.empty) 
      Just ( Empty , _ ) -> error "permutationsBinarySequences: should not happen"
    findj :: (Seq Bool, Seq Bool) -> Maybe (Seq Bool, Seq Bool)   
    findj ( xxs@(x:<|xs), yys@(_:<|_) ) = if x 
      then findj ( xs, True <| yys )
      else Just ( xxs, yys )
    findj ( x:<|xs, Empty) = findj( xs, S.singleton x)
    findj ( Empty , _ ) = Nothing
    inc :: Bool -> Seq Bool -> (Seq Bool, Seq Bool) -> Seq Bool
    inc !u us ( x:<|xs , yys ) = if u
      then inc True us ( xs , x <| yys ) 
      else (><) (S.reverse (True <| us)) ((><) (S.reverse (u <| yys)) xs)
    inc _ _ ( Empty , _ ) = error "permutationsBinarySequences: should not happen"

-- | Elementary symmetric polynomial
esPolynomial 
  :: (AlgRing.C a, Eq a) 
  => Int -- ^ number of variables
  -> Int -- ^ index
  -> Spray a
esPolynomial n k
  | k <= 0 || n <= 0 = error "both arguments must be positive integers"
  | k > n = AlgAdd.zero
  | otherwise = simplifySpray spray
  where
    perms = permutationsBinarySequences (n-k) k
    spray = HM.fromList $ map (\expts -> (Powers expts n, AlgRing.one)) perms

-- | number of variables
numberOfVariables :: Spray a -> Int
numberOfVariables spray = maximum (map nvariables powers)
  where
    powers = HM.keys spray

-- | Whether a spray is a symmetric polynomial
isSymmetricSpray :: forall a. (AlgField.C a, Eq a) => Spray a -> Bool
isSymmetricSpray spray = check1 && check2 
  where
    n = numberOfVariables spray
    indices = [1 .. n]
    esPolys = map (\i -> esPolynomial n i :: Spray a) indices
    yPolys = map (\i -> lone (n + i) :: Spray a) indices
    gPolys = zipWith (^-^) esPolys yPolys
    gbasis = groebner gPolys
    g = sprayDivision spray gbasis
    gpowers = HM.keys g
    check1 = minimum (map nvariables gpowers) > n
    expnts = map exponents gpowers
    check2 = DF.all (DF.all (0 ==)) (map (S.take n) expnts) 

