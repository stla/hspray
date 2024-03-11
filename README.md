# hspray

<!-- badges: start -->
[![Stack-lts](https://github.com/stla/hspray/actions/workflows/Stack-lts.yml/badge.svg)](https://github.com/stla/hspray/actions/workflows/Stack-lts.yml)
[![Stack-nightly](https://github.com/stla/hspray/actions/workflows/Stack-nightly.yml/badge.svg)](https://github.com/stla/hspray/actions/workflows/Stack-nightly.yml)
<!-- badges: end -->

Simple multivariate polynomials in Haskell.

___


```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ (4 *^ (x ^*^ y ^*^ z))
prettySpray show "X" poly
-- "(4.0) * X^(3, 1, 1) + (8.0) * X^(4, 2, 2)"
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
prettySpray show "X" poly
-- "(7 % 4) * X^(3, 1, 1) + (7 % 6) * X^(4, 2, 2)"
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

Differentiation:

```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = 2 *^ (x ^*^ y ^*^ z) ^+^ (3 *^ x^**^2)
-- derivate with respect to x
prettySpray show "X" $ derivSpray 1 poly
-- "(2.0) * X^(0, 1, 1) + (6.0) * X^(1)"
```

## Grôbner bases

As of version 2.0.0, it is possible to compute a Gröbner basis.

```haskell
import Math.Algebra.Hspray
import Data.Ratio
-- define the elementary monomials
o = lone 0 :: Spray Rational
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
-- define three polynomials
p1 = x^**^2 ^+^ y ^+^ z ^-^ o -- X² + Y + Z - 1
p2 = x ^+^ y^**^2 ^+^ z ^-^ o -- X + Y² + Z - 1
p3 = x ^+^ y ^+^ z^**^2 ^-^ o -- X + Y + Z² - 1
-- compute the Gröbner basis
gbasis = groebner [p1, p2, p3]
-- show result
prettyResult = map prettySprayXYZ gbasis
mapM_ print prettyResult
-- "((-1) % 1) + (1 % 1) Z^2 + (1 % 1) Y + (1 % 1) X"
-- "(1 % 1) Z + ((-1) % 1) Z^2 + ((-1) % 1) Y + (1 % 1) Y^2"
-- "((-1) % 2) Z^2 + (1 % 2) Z^4 + (1 % 1) YZ^2"
-- "((-1) % 1) Z^2 + (4 % 1) Z^3 + ((-4) % 1) Z^4 + (1 % 1) Z^6"
```


## Easier usage 

To construct a polynomial using the ordinary symbols `+`, `*` and `-`, 
one can hide these operators from **Prelude** and import them from 
the **numeric-prelude** library:

```haskell
import Prelude hiding ((*), (+), (-))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring                  
import Math.Algebra.Hspray
```

Or, maybe better (I didn't try yet), follow the "Usage" section on the 
[Hackage page](https://hackage.haskell.org/package/numeric-prelude-0.4.4#usage) 
of **numeric-prelude**.

## Symbolic coefficients

Assume you have the polynomial `a * (x² + y²) + 2b/3 * z`, 
where `a` and `b` are symbolic coefficients. 
You can define this polynomial as a `Spray` as follows:

```haskell
import Prelude hiding ((*), (+), (-))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring                  
import Math.Algebra.Hspray
import Data.Ratio

x = lone 1 :: Spray (Spray Rational)
y = lone 2 :: Spray (Spray Rational)
z = lone 3 :: Spray (Spray Rational)
a = lone 1 :: Spray Rational
b = lone 2 :: Spray Rational

poly = a *^ (x*x + y*y) + ((2%3) *^ b) *^ z 
prettySpray (prettySpray show "a") "X" poly
-- "((2 % 3) * a^(0, 1)) * X^(0, 0, 1) + ((1 % 1) * a^(1)) * X^(0, 2) + ((1 % 1) * a^(1)) * X^(2)"
```

The `prettySpray` function shows the expansion of the polynomial. 
You can extract the powers and the coefficients as follows:

```haskell
l = toList poly
map fst l
-- [[0,0,1],[2],[0,2]]
map toList $ map snd l
-- [[([0,1],2 % 3)],[([1],1 % 1)],[([1],1 % 1)]]
```
