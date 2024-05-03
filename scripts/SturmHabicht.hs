import Math.Algebra.Hspray

x = qlone 1
y = qlone 2

p = x^**^3^*^y^**^4 ^+^ x^**^2^*^y^**^2 ^+^ 3*^y ^-^ constantSpray 6
sh1 = sturmHabichtSequence 1 p
sh2 = sturmHabichtSequence 2 p
psh1 = principalSturmHabichtSequence 1 p
psh2 = principalSturmHabichtSequence 2 p



