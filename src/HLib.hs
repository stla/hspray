{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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

cleanSpray :: (AlgAdd.C a, Eq a) => Spray a -> Spray a
cleanSpray p = HM.filter (/= AlgAdd.zero) p

f :: AlgAdd.C a => Spray a -> Seq Int -> a -> Spray a
f s powers coef = HM.insertWith (AlgAdd.+) powers coef s

addSprays :: (AlgAdd.C a, Eq a) => Spray a -> Spray a -> Spray a
addSprays p q = cleanSpray $ HM.foldlWithKey' f p q

negateSpray :: AlgAdd.C a => Spray a -> Spray a
negateSpray p = HM.map AlgAdd.negate p

p1 :: Spray Double
p1 = HM.fromList [(S.fromList [1, 0], 2)]

p2 :: Spray Double
p2 = HM.fromList [(S.fromList [1, 1], 3)]

p :: Spray Double
p = p1 AlgAdd.+ p2
