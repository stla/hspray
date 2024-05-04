{-# LANGUAGE FlexibleContexts #-}
import qualified Algebra.Absolute as AlgAbs
import qualified Algebra.Additive as AlgAdd
import qualified Algebra.Ring     as AlgRing
import qualified Algebra.ToRational as AlgToRational
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

_signVariations :: Eq a => (a -> Char) -> [a] -> Int
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

wStHa :: (Eq a, AlgRing.C a) => ([a] -> Int) -> Spray a -> (a, a) -> Int
wStHa signVariationsFunc spray (alpha, beta) = 
  signVariationsFunc galpha - signVariationsFunc gbeta 
  where
    g = filter (not . isZeroSpray) (sturmHabichtSequence 1 spray)
    galpha = map (evaluateAt [alpha]) g
    gbeta  = map (evaluateAt [beta]) g

numberOfRealRootsInInterval :: 
  (Eq a, Num a, AlgRing.C a) => Spray a -> (a, a) -> Int
numberOfRealRootsInInterval spray = 
  if isUnivariate spray 
    then wStHa signVariations spray
    else error "numberOfRealRootsInInterval: the spray is not univariate."

numberOfRealRootsInInterval' :: (Eq a, AlgAbs.C a) => Spray a -> (a, a) -> Int
numberOfRealRootsInInterval' spray =
  if isUnivariate spray 
    then wStHa signVariations' spray
    else error "numberOfRealRootsInInterval': the spray is not univariate."
 

x = qlone 1
factors = [x ^-^ constantSpray (toRational i) | i <- [1::Int .. 3]]
test = AlgRing.product factors
