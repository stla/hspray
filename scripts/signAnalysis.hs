{-# LANGUAGE FlexibleContexts #-}
import qualified Algebra.Absolute as AlgAbs
import qualified Algebra.Additive as AlgAdd
import qualified Algebra.Ring     as AlgRing

runLengthEncoding :: Eq a => [a] -> [(a,Int)]
runLengthEncoding = foldr code []
  where 
    code c []         = [(c,1)]
    code c ((x,n):ts) 
      | c == x        = (x,n+1):ts
      | otherwise     = (c,1):(x,n):ts

signPermancesAndVariations :: (Eq a, AlgRing.C a, AlgAbs.C a) => [a] -> (Int, Int)
signPermancesAndVariations as = (permanences, variations)
  where
    sign a = if AlgAbs.signum a == AlgRing.one then '+' else '-'
    signs = map sign as
    rle = runLengthEncoding signs
    l = length rle
    lengths = map snd rle
    permanences = sum lengths - l
    variations = l - 1

signVariations' :: forall a. (Eq a, AlgRing.C a, AlgAbs.C a) => [a] -> Int
signVariations' as = v1 + v2 + 2*v3
  where
    count x xs = sum (map (fromEnum . (== x)) xs)
    l = length as
    sign :: a -> Char
    sign a
      | AlgAbs.signum a == AlgAdd.zero = '0'
      | AlgAbs.signum a == AlgRing.one = '+'
      | otherwise                      = '-' 
    signs = map sign as
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


