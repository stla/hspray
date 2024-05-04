import qualified Algebra.Absolute as AlgAbs
import qualified Algebra.Additive as AlgAdd
import qualified Algebra.Ring     as AlgRing
import Data.List.Extra (unsnoc)
import Data.Maybe (fromJust)
import Math.Algebra.Hspray

runLengthEncoding :: Eq a => [a] -> [(a,Int)]
runLengthEncoding = foldr code []
  where 
    code c []         = [(c,1)]
    code c ((x,n):ts) 
      | c == x        = (x,n+1):ts
      | otherwise     = (c,1):(x,n):ts

_signPermanencesAndVariations :: Eq a => (a -> Char) -> [a] -> (Int, Int)
_signPermanencesAndVariations signFunc as = (permanences, variations)
  where
    signs = map signFunc as
    rle = runLengthEncoding signs
    l = length rle
    lengths = map snd rle
    permanences = sum lengths - l
    variations = l - 1

signPermanencesAndVariations :: (Eq a, Num a) => [a] -> (Int, Int)
signPermanencesAndVariations = _signPermanencesAndVariations signFunc
  where
    signFunc a = if signum a == 1 then '+' else '-'

signPermanencesAndVariations' :: (Eq a, AlgAbs.C a) => [a] -> (Int, Int)
signPermanencesAndVariations' = _signPermanencesAndVariations signFunc
  where
    signFunc a = if AlgAbs.signum a == AlgRing.one then '+' else '-'

{- _signVariations :: Eq a => (a -> Char) -> [a] -> Int
_signVariations signFunc as = v1 + v2 + 2*v3
  where
    count x xs = sum (map (fromEnum . (== x)) xs)
    l = length as
    signs = map signFunc as
    chunks2 = [(signs !! i, signs !! (i+1)) | i <- [0 .. l-2]]
    v1 = count ('+', '-') chunks2
           + count ('-', '+') chunks2
    chunks3 = [(signs !! i, signs !! (i+1), signs !! (i+2)) | i <- [0 .. l-3]]
    chunks4 = [(signs !! i, signs !! (i+1), signs !! (i+2), signs !! (i+3)) 
                | i <- [0 .. l-4]]
    v2 = count ('-', '0', '+') chunks3 +
           count ('+', '0', '-') chunks3 +
             count ('+', '0', '0', '-') chunks4 +
               count ('-', '0', '0', '+') chunks4
    v3 = count ('+', '0', '0', '+') chunks4 + 
           count ('-', '0', '0', '-') chunks4

signVariations :: (Eq a, Num a) => [a] -> Int
signVariations = _signVariations signFunc
  where
    signFunc a
      | signum a == 0 = '0'
      | signum a == 1 = '+'
      | otherwise     = '-' 

signVariations' :: (Eq a, AlgAbs.C a) => [a] -> Int
signVariations' = _signVariations signFunc
  where
    signFunc a
      | AlgAbs.signum a == AlgAdd.zero = '0'
      | AlgAbs.signum a == AlgRing.one = '+'
      | otherwise                      = '-' 

_numberOfRealRootsInOpenInterval :: 
  (Eq a, AlgRing.C a, Ord a) => ([a] -> Int) -> Spray a -> (a, a) -> Int
_numberOfRealRootsInOpenInterval signVariationsFunc spray (alpha, beta) 
  | alpha == beta = 0
  | isConstantSpray spray = if isZeroSpray spray 
    then error "numberOfRealRoots: the spray is null."
    else 0
  | otherwise = if sprayAtBeta == AlgAdd.zero then svDiff - 1 else svDiff
  where
    (alpha', beta') = if alpha < beta then (alpha, beta) else (beta, alpha)
    (ginit, glast) = 
      fromJust $ unsnoc $ filter (not . isZeroSpray) (sturmHabichtSequence 1 spray)
    sprayAtAlpha = evaluateAt [alpha] glast
    sprayAtBeta  = evaluateAt [beta] glast
    galpha = map (evaluateAt [alpha]) ginit ++ [sprayAtAlpha]
    gbeta  = map (evaluateAt [beta]) ginit ++ [sprayAtBeta]
    svalpha = signVariationsFunc galpha
    svbeta  = signVariationsFunc gbeta
    svDiff  = svalpha - svbeta

_numberOfRealRootsInClosedInterval :: 
  (Eq a, AlgRing.C a, Ord a) => ([a] -> Int) -> Spray a -> (a, a) -> Int
_numberOfRealRootsInClosedInterval signVariationsFunc spray (alpha, beta) = 
  if alpha == beta 
    then 
      fromEnum (sprayAtAlpha == AlgAdd.zero)
    else 
      _numberOfRealRootsInOpenInterval signVariationsFunc spray (alpha, beta) +
        fromEnum (sprayAtAlpha == AlgAdd.zero) + 
          fromEnum (sprayAtBeta == AlgAdd.zero)
  where
    sprayAtAlpha = evaluateAt [alpha] spray
    sprayAtBeta  = evaluateAt [beta] spray
 
-- | Number of real roots of a spray in an open interval (that makes sense 
-- only for a spray on a ring embeddable in the real numbers).
numberOfRealRootsInOpenInterval :: 
  (Eq a, Num a, AlgRing.C a, Ord a) => Spray a -> (a, a) -> Int
numberOfRealRootsInOpenInterval spray = 
  if isUnivariate spray 
    then _numberOfRealRootsInOpenInterval signVariations spray
    else error "numberOfRealRootsInOpenInterval: the spray is not univariate."

-- | Number of real roots of a spray in a closed interval (that makes sense 
-- only for a spray on a ring embeddable in the real numbers).
numberOfRealRootsInClosedInterval :: 
  (Eq a, Num a, AlgRing.C a, Ord a) => Spray a -> (a, a) -> Int
numberOfRealRootsInClosedInterval spray = 
  if isUnivariate spray 
    then _numberOfRealRootsInClosedInterval signVariations spray
    else error "numberOfRealRootsInClosedInterval: the spray is not univariate."
 
-- | Number of real roots of a spray in an open interval (that makes sense 
-- only for a spray on a ring embeddable in the real numbers).
numberOfRealRootsInOpenInterval' :: 
  (Eq a, AlgAbs.C a, Ord a) => Spray a -> (a, a) -> Int
numberOfRealRootsInOpenInterval' spray =
  if isUnivariate spray 
    then _numberOfRealRootsInOpenInterval signVariations' spray
    else error "numberOfRealRootsInOpenInterval': the spray is not univariate."

-- | Number of real roots of a spray in a closed interval (that makes sense 
-- only for a spray on a ring embeddable in the real numbers).
numberOfRealRootsInClosedInterval' :: 
  (Eq a, AlgAbs.C a, Ord a) => Spray a -> (a, a) -> Int
numberOfRealRootsInClosedInterval' spray =
  if isUnivariate spray 
    then _numberOfRealRootsInClosedInterval signVariations' spray
    else error "numberOfRealRootsInClosedInterval': the spray is not univariate."

 -}
x = qlone 1
factors = [x ^-^ constantSpray (toRational i) | i <- [1::Int .. 3]]
spray = AlgRing.product factors

test = map (numberOfRealRootsInClosedInterval spray) [(0, 10), (0, 5), (2, 3)]


distributionsOfZeros :: (Eq a, AlgAdd.C a) => [a] -> ([Int], [Int], [Int])
distributionsOfZeros as = (i_, k_, ik_)
  where
    symbol a = if a == AlgAdd.zero then '0' else '*'
    symbols = map symbol as ++ ['0']
    lengths = map snd (runLengthEncoding symbols)
    l = length lengths
    cumulativeLengths = scanl (+) 0 lengths
    range = [0 .. l-1]
    ik_ = [cumulativeLengths !! i | i <- range, even i]
    i_ = [cumulativeLengths !! i - 1 | i <- range, odd i]
    k_ = [lengths !! i | i <- [0 .. l-2], odd i] ++ [lengths !! (l-1) - 1]

s = [2, 0, 0, 3, 4, 0, 5, 0, 0] :: [Int]

_blocksAndEpsilons :: (Eq a, AlgAdd.C a) => (a -> Int) -> [a] -> ([[a]], [Int])
_blocksAndEpsilons signFunc as = (blocks, epsilons)
  where
    (i_, k_, ik_) = distributionsOfZeros as
    t = length i_
    blocks = [[as !! m | m <- [ik_ !! n .. i_ !! n]] | n <- [0 .. t-1]]
    epsilon s = let ks = k_ !! s in if odd ks 
      then
        0
      else
        (if even (ks `div` 2) then 1 else -1) * 
          signFunc (as !! (ik_ !! (s+1))) * signFunc (as !! (i_ !! s))
    epsilons = [epsilon n | n <- [0 .. t-2]]

blocksAndEpsilons :: (Eq a, AlgAdd.C a, Num a) => [a] -> ([[a]], [Int])
blocksAndEpsilons = _blocksAndEpsilons signFunc 
  where
    signFunc a = if signum a == 1 then 1 else -1

blocksAndEpsilons' :: (Eq a, AlgAbs.C a) => [a] -> ([[a]], [Int])
blocksAndEpsilons' = _blocksAndEpsilons signFunc 
  where
    signFunc a = if AlgAbs.signum a == AlgRing.one then 1 else -1

_C :: (Eq a, AlgAdd.C a) => ([a] -> (Int, Int)) -> ([[a]], [Int]) -> Int
_C signPermanencesAndVariationsFunc (blocks, epsilons) = sum pvs - sum epsilons
  where
    permanencesMinusVariations as = 
      let (p, v) = signPermanencesAndVariationsFunc as in p - v 
    pvs = map permanencesMinusVariations blocks

numberOfRealRoots :: (Eq a, AlgRing.C a, Num a) => Spray a -> Int
numberOfRealRoots spray = 
  if isUnivariate spray 
    then 
      _C signPermanencesAndVariations (blocksAndEpsilons $ reverse as)
    else 
      error "numberOfRealRoots: the spray is not univariate."
  where
    as = map getConstantTerm (principalSturmHabichtSequence 1 spray)

numberOfRealRoots' :: (Eq a, AlgAbs.C a) => Spray a -> Int
numberOfRealRoots' spray = 
  if isUnivariate spray 
    then 
      _C signPermanencesAndVariations' (blocksAndEpsilons' $ reverse as)
    else 
      error "numberOfRealRoots: the spray is not univariate."
  where
    as = map getConstantTerm (principalSturmHabichtSequence 1 spray)
