{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HLib
    where
import qualified Algebra.Additive as AlgAdd
import qualified Algebra.Module   as AlgMod
import qualified Algebra.Ring     as AlgRing
import           Data.Foldable    ( toList )
import qualified Data.Sequence    as S
import           Data.Sequence    ( Seq, (><), (|>) )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


type Spray a = HashMap (Seq Int) a

instance (AlgAdd.C a, Eq a) => AlgAdd.C (Spray a) where
    p + q = addSprays p q
    zero = HM.empty
    negate = negateSpray

instance (AlgMod.C a a, Eq a) => AlgMod.C a (Spray a) where
  lambda *> p = scaleSpray lambda p

instance (AlgRing.C a, Eq a) => AlgRing.C (Spray a) where
    p * q = multSprays p q
    one = lone 0


cleanSpray :: (AlgAdd.C a, Eq a) => Spray a -> Spray a
cleanSpray = HM.filter (/= AlgAdd.zero)

addSprays :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
addSprays p q = cleanSpray $ HM.foldlWithKey' f p q
    where
        f s powers coef = HM.insertWith (AlgAdd.+) powers coef s

negateSpray :: AlgAdd.C a => Spray a -> Spray a
negateSpray = HM.map AlgAdd.negate 

scaleSpray :: AlgMod.C a a => a -> Spray a -> Spray a
scaleSpray lambda = HM.map (lambda AlgMod.*>)

growSequence :: Seq Int -> Int -> Seq Int 
growSequence s n = s >< t
  where 
    m = S.length s 
    t = S.replicate (n - m) 0

multMonomial :: AlgRing.C a => (Seq Int, a) -> (Seq Int, a) -> (Seq Int, a)
multMonomial (pows1, coef1) (pows2, coef2) = 
    (S.zipWith (+) pows1' pows2', coef1 AlgRing.* coef2) 
  where
    n1 = S.length pows1
    n2 = S.length pows2
    (pows1', pows2') = if n1 > n2 
        then 
            (pows1, growSequence pows2 n1)
        else
            (growSequence pows1 n2, pows2)

multSprays :: (AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
multSprays p q = cleanSpray $ HM.fromListWith (AlgAdd.+) prods
    where
        p' = HM.toList p
        q' = HM.toList q
        prods = [multMonomial mp mq | mp <- p', mq <- q']

-- | Polynomial x_n
lone :: AlgRing.C a => Int -> Spray a
lone n = HM.singleton pows AlgRing.one
  where
    pows = if n == 0 
      then 
        S.empty 
      else 
        S.replicate (n - 1) AlgAdd.zero |> AlgRing.one

p1 :: Spray Double
p1 = HM.fromList [(S.fromList [1, 0], 2)]

p2 :: Spray Double
p2 = HM.fromList [(S.fromList [1, 1], 3)]

p :: Spray Double
p = p1 AlgRing.* p2
