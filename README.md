# hspray

Simple multivariate polynomials in Haskell.

___


```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ (4 *^ (x ^*^ y ^*^ z))
prettySpray show "x" poly
-- "(4.0) * x^(3, 1, 1) + (8.0) * x^(4, 2, 2)"
```

More generally, one can use the type `Spray a` as long as the type `a` has 
the instances `Eq` and `Algebra.Ring` (defined in the **numeric-prelude** 
library). For example `a = Rational`:

```haskell
import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
poly = ((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
prettySpray show "x" poly
-- "(7 % 4) * x^(3, 1, 1) + (7 % 6) * x^(4, 2, 2)"
```

Or `a = Spray Double`:

```haskell
import Math.Algebra.Hspray
p = lone 1 :: Spray Double
x = lone 1 :: Spray (Spray Double)
y = lone 2 :: Spray (Spray Double)
poly = ((p *^ x) ^+^ (p *^ y))^**^2  
prettySpray (prettySpray show "a") "X" poly
-- "((1.0) * a^(2)) * X^(0, 2) + ((2.0) * a^(2)) * X^(1, 1) + ((1.0) * a^(2)) * X^(2)"
```

Evaluation:

```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = 2 *^ (x ^*^ y ^*^ z) 
-- evaluate poly at x=2, y=1, z=2
evalSpray poly [2, 1, 2]
-- 8.0
```

