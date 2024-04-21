import Math.Algebra.Hspray

type PQS = ParametricQSpray

f :: (QSpray, QSpray) -> (PQS, PQS, PQS) -> PQS
f (a, b) (x, y, z) = (a %:% (a ^+^ b)) *^ x^**^2  ^+^  (b %:% (a ^+^ b)) *^ (y ^*^ z)

a = qlone 1
b = qlone 2
x = lone 1 :: PQS
y = lone 2 :: PQS
z = lone 3 :: PQS
pqs = f (a, b) (x, y, z)
