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
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


data Powers = Powers 
  {
    exponents :: Seq Int,
    nvariables :: Int
  }
  deriving Show

growSequence :: Seq Int -> Int -> Int -> Seq Int 
growSequence s m n = s >< t
  where 
    t = S.replicate (n - m) 0

harmonize :: (Powers, Powers) -> (Powers, Powers)
harmonize (pows1, pows2) = (Powers e1' n, Powers e2' n)
  where
    e1 = exponents pows1
    e2 = exponents pows2
    n1 = nvariables pows1
    n2 = nvariables pows2
    (e1', e2', n) = if n1 < n2
      then
        (growSequence e1 n1 n2, e2, n2)
      else
        (e1, growSequence e2 n2 n1, n1)

instance Eq Powers where
  pows1 == pows2 = (exponents pows1') == (exponents pows2')
    where
      (pows1', pows2') = harmonize (pows1, pows2)

instance Hashable Powers where 
  hashWithSalt k pows = hashWithSalt k (exponents pows, nvariables pows)

type Spray a = HashMap Powers a

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

multMonomial :: AlgRing.C a => (Powers, a) -> (Powers, a) -> (Powers, a)
multMonomial (pows1, coef1) (pows2, coef2) = 
    (pows, coef1 AlgRing.* coef2) 
  where
    (pows1', pows2') = harmonize (pows1, pows2)
    expts = S.zipWith (+) (exponents pows1') (exponents pows2')
    pows = Powers expts (nvariables pows1') 

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
        Powers S.empty 0 
      else 
        Powers (S.replicate (n - 1) AlgAdd.zero |> AlgRing.one) n

p1 :: Spray Double
p1 = HM.fromList [(Powers (S.fromList [1, 0]) 2, 2)]

p2 :: Spray Double
p2 = HM.fromList [(Powers (S.fromList [1, 1]) 2, 3)]

p :: Spray Double
p = p1 AlgRing.* p2
