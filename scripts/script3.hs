import Math.Algebra.Hspray

type SPQS = SimpleParametricQSpray

f :: (QSpray, QSpray) -> (SPQS, SPQS, SPQS) -> SPQS
f (a, b) (x, y, z) = 
  (a ^+^ b) *^ x^**^2  ^+^  (a^**^2 ^+^ b^**^2) *^ (y ^*^ z)

a = qlone 1
b = qlone 2
x = lone 1 :: SPQS
y = lone 2 :: SPQS
z = lone 3 :: SPQS
spqs = f (a, b) (x, y, z)

s = prettySimpleParametricQSprayABCXYZ ["a","b"] ["X","Y","Z"] spqs
