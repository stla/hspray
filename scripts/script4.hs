import Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = 2 *^ (x ^*^ y ^*^ z) 
-- evaluate poly at x=2, y=1, z=2
evalSpray poly [2, 1, 2]
-- 8.0
