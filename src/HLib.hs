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


infixr 7 *^

infixl 6 ^+^, ^-^

infixl 7 ^*^ 

infixr 8 ^**^


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

-- | Addition of two polynomials
(^+^) :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
(^+^) p q = p AlgAdd.+ q

-- | Substraction
(^-^) :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
(^-^) p q = p AlgAdd.- q

-- | Multiply two polynomials
(^*^) :: (AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
(^*^) p q = p AlgRing.* q

-- | Power of a polynomial
(^**^) :: (AlgRing.C a, Eq a) => Spray a -> Int -> Spray a
(^**^) p n = foldl1 (^*^) (replicate n p) 

-- | Scale polynomial by a scalar
(*^) :: (AlgMod.C a a, Eq a) => a -> Spray a -> Spray a
(*^) lambda pol = lambda AlgMod.*> pol 


simplifyPowers :: Powers -> Powers
simplifyPowers pows = Powers s (S.length s)
  where
    s = S.dropWhileR (==0) (exponents pows)

simplifySpray :: Spray a -> Spray a
simplifySpray p = HM.mapKeys simplifyPowers p

cleanSpray :: (AlgAdd.C a, Eq a) => Spray a -> Spray a
cleanSpray p = HM.filter (/= AlgAdd.zero) (simplifySpray p)

addSprays :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
addSprays p q = cleanSpray $ HM.foldlWithKey' f p q
    where
        f s powers coef = HM.insertWith (AlgAdd.+) powers coef s

negateSpray :: AlgAdd.C a => Spray a -> Spray a
negateSpray = HM.map AlgAdd.negate 

scaleSpray :: (AlgMod.C a a, Eq a) => a -> Spray a -> Spray a
scaleSpray lambda p = cleanSpray $ HM.map (lambda AlgMod.*>) p

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

constantSpray :: (AlgMod.C a a, AlgRing.C a, Eq a) => a -> Spray a
constantSpray c = c *^ (lone 0)

evalMonomial :: AlgRing.C a => [a] -> (Powers, a) -> a
evalMonomial xyz (powers, coeff) =
  coeff AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows)
  where
    pows = toList (fromIntegral <$> exponents powers)

evalSpray :: AlgRing.C a => Spray a -> [a] -> a
evalSpray p xyz = AlgAdd.sum $ map (evalMonomial xyz) (HM.toList p)

identify :: (AlgMod.C a a, AlgRing.C a, Eq a) => Spray a -> Spray (Spray a)
identify p = HM.map constantSpray p

composeSprays :: (AlgMod.C a a, AlgRing.C a, Eq a) => Spray a -> Spray a -> Spray a
composeSprays p q = evalSpray (identify p) (map (uncurry HM.singleton) (HM.toList q))

fromList :: (AlgRing.C a, Eq a) => [([Int], a)] -> Spray a
fromList x = cleanSpray $ HM.fromList $ map (\(expts, coef) -> (Powers (S.fromList expts) (length expts), coef)) x

p1 :: Spray Double
p1 = fromList [([1, 0], 2)]

p2 :: Spray Double
p2 = HM.fromList [(Powers (S.fromList [1, 1]) 2, 3)]

p :: Spray Double
p = p1 AlgRing.* p2
