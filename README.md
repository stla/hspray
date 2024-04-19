# hspray

<!-- badges: start -->
[![Stack-lts](https://github.com/stla/hspray/actions/workflows/Stack-lts.yml/badge.svg)](https://github.com/stla/hspray/actions/workflows/Stack-lts.yml)
[![Stack-nightly](https://github.com/stla/hspray/actions/workflows/Stack-nightly.yml/badge.svg)](https://github.com/stla/hspray/actions/workflows/Stack-nightly.yml)
<!-- badges: end -->

*Simple multivariate polynomials in Haskell.*

___

An object of type `Spray a` represents a multivariate polynomial whose
coefficients are represented by the type `a`. For example:

```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ (4 *^ (x ^*^ y ^*^ z))
putStrLn $ prettyNumSpray poly
-- 8.0*x^4.y^2.z^2 + 4.0*x^3.y.z
```

This is the easiest way to construct a spray: first introduce the polynomial 
variables with the `lone` function, and then use arithmetic operations.

There are numerous functions to print a spray. If you don't like the letters 
`x`, `y`, `z` in the output of `prettyNumSpray`, you can use `prettyNumSprayXYZ` 
to change them to whatever you want:

```haskell
putStrLn $ prettyNumSprayXYZ ["A","B","C"] poly
-- 8.0*A^4.B^2.C^2 + 4.0*A^3.B.C
```

Note that this function does not throw an error if you don't provide enough 
letters:

```haskell
putStrLn $ prettyNumSprayXYZ ["A","B"] poly
-- 8.0*A1^4.A2^2.A3^2 + 4.0*A1^3.A2.A3
```

This is the same output as the one of `prettyNumSprayX1X2X3 "A" poly`.

More generally, one can use the type `Spray a` as long as the type `a` has 
the instances `Eq` and `Algebra.Ring` (defined in the **numeric-prelude** 
library). For example `a = Rational`:

```haskell
import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: QSpray -- QSpray = Spray Rational
y = lone 2 :: QSpray 
z = lone 3 :: QSpray
poly = ((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^-^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
putStrLn $ prettyQSpray poly
-- (7/6)*x^4.y^2.z^2 - (7/4)*x^3.y.z
```

Or `a = Spray Double`:

```haskell
import Math.Algebra.Hspray
alpha = lone 1 :: Spray Double
x = lone 1 :: Spray (Spray Double)
y = lone 2 :: Spray (Spray Double)
poly = ((alpha *^ x) ^+^ (alpha *^ y))^**^2  
showSprayXYZ' (prettyNumSprayXYZ ["alpha"]) ["x","y"] poly
-- (alpha^2)*x^2 + (2.0*alpha^2)*x.y + (alpha^2)*y^2
```

#### Evaluation:

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

#### Partial evaluation:

```haskell
import Math.Algebra.Hspray
import Data.Ratio
x1 = lone 1 :: Spray Rational
x2 = lone 2 :: Spray Rational
x3 = lone 3 :: Spray Rational
poly = x1^**^2 ^+^ x2 ^+^ x3 ^-^ unitSpray
putStrLn $ prettyQSprayX1X2X3 "x" poly
-- x1^2 + x2 + x3 - 1
--
-- substitute x1 -> 2 and x3 -> 3
poly' = substituteSpray [Just 2, Nothing, Just 3] poly
putStrLn $ prettyQSprayX1X2X3 "x" poly'
-- x2 + 6
```

#### Differentiation:

```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
poly = 2 *^ (x ^*^ y ^*^ z) ^+^ (3 *^ x^**^2)
putStrLn $ prettyNumSpray poly
-- 3.0*x^2 + 2.0*x.y.z
--
-- derivative with respect to x
putStrLn $ prettyNumSpray $ derivative 1 poly
-- 6.0*x + 2.0*y.z"
```

## Gröbner bases

As of version 2.0.0, it is possible to compute a Gröbner basis.

```haskell
import Math.Algebra.Hspray
import Data.Ratio
-- define the elementary monomials
o = lone 0 :: Spray Rational -- same as unitSpray
x = lone 1 :: Spray Rational
y = lone 2 :: Spray Rational
z = lone 3 :: Spray Rational
-- define three polynomials
p1 = x^**^2 ^+^ y ^+^ z ^-^ o -- X² + Y + Z - 1
p2 = x ^+^ y^**^2 ^+^ z ^-^ o -- X + Y² + Z - 1
p3 = x ^+^ y ^+^ z^**^2 ^-^ o -- X + Y + Z² - 1
-- compute the reduced Gröbner basis
gbasis = groebner [p1, p2, p3] True
-- show result
prettyResult = map prettyQSpray gbasis
mapM_ print prettyResult
-- "x + y + z^2 - 1"
-- "y^2 - y - z^2 + z"
-- "y.z^2 + (1/2)*z^4 - (1/2)*z^2"
-- "z^6 - 4*z^4 + 4*z^3 - z^2"
```


## Easier usage 

To construct a spray using the ordinary symbols `+`, `-`, `*` and `^`, 
one can hide these operators from **Prelude** and import them from the 
**numeric-prelude** library; constructing a spray in this context is easier:

```haskell
import Prelude hiding ((+), (-), (*), (^), (*>), (<*))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring                  
import Math.Algebra.Hspray
import Data.Ratio
x = lone 1 :: QSpray 
y = lone 2 :: QSpray 
z = lone 3 :: QSpray
poly  = ((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^-^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
poly' = ((2%3) *^ (x^3 * y * z) - x^2) * ((7%4) *^ (x * y * z))
poly == poly'
-- True
```

Note that `*>` could be used instead of `*^` but running `lambda *> spray` 
possibly throws an "ambiguous type" error regarding the type of `lambda`.

Maybe better (I didn't try yet), follow the "Usage" section on the 
[Hackage page](https://hackage.haskell.org/package/numeric-prelude-0.4.4#usage) 
of **numeric-prelude**.


## Symbolic coefficients

Assume you have the polynomial `a * (x² + y²) + 2b/3 * z`, 
where `a` and `b` are symbolic rational numbers. You can represent this 
polynomial by a `Spray (Spray Rational)` spray as follows:

```haskell
import Prelude hiding ((*), (+), (-), (^))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Ring                  
import Math.Algebra.Hspray

x = lone 1 :: Spray (Spray Rational)
y = lone 2 :: Spray (Spray Rational)
z = lone 3 :: Spray (Spray Rational)
a = lone 1 :: Spray Rational
b = lone 2 :: Spray Rational

poly = a *^ (x^2 + y^2) + ((2 *^ b) /^ 3) *^ z 
putStrLn $ 
  showSprayXYZ' (prettyQSprayXYZ ["a","b"]) ["X","Y","Z"] poly
-- (a)*X^2 + (a)*Y^2 + ((2/3)*b)*Z
```

You can extract the powers and the coefficients as follows:

```haskell
l = toList poly
map fst l
-- [[0,0,1],[2],[0,2]]
map toList $ map snd l
-- [[([0,1],2 % 3)],[([1],1 % 1)],[([1],1 % 1)]]
```

These `Spray (Spray a)` sprays can be very useful. They represent polynomials 
whose coefficients depend on some parameters, with a polynomial dependence. 
For example, the coefficients of the 
[Gegenbauer polynomials](https://en.wikipedia.org/wiki/Gegenbauer_polynomials)
are polynomials in their parameter $\alpha$ (this is clear from the recurrence 
relation). Here is their implementation in **hspray**:

```haskell
gegenbauerPolynomial :: Int -> Spray (Spray Rational) 
gegenbauerPolynomial n 
  | n == 0 = unitSpray
  | n == 1 = (2.^a) *^ x
  | otherwise = 
    (2.^(n'' ^+^ a) /^ n') *^ (x ^*^ gegenbauerPolynomial (n - 1))
    ^-^ ((n'' ^+^ 2.^a ^-^ unitSpray) /^ n') *^ gegenbauerPolynomial (n - 2)
  where 
    x = lone 1 :: Spray (Spray Rational)
    a = lone 1 :: Spray Rational
    n'  = toRational n
    n'' = constantSpray (n' - 1)
```

Let's try it:

```haskell
n = 3
g = gegenbauerPolynomial n
putStrLn $ 
  showSprayXYZ' (prettyQSprayXYZ ["alpha"]) ["X"] g
-- ((4/3)*alpha^3 + 4*alpha^2 + (8/3)*alpha)*X^3 + (-2*alpha^2 - 2*alpha)*X
```

Let's check the differential equation:

```haskell
g'  = derivSpray 1 g
g'' = derivSpray 1 g'
alpha = lone 1 :: Spray Rational
x     = lone 1 :: Spray (Spray Rational)
nAsSpray = constantSpray (toRational n)
shouldBeZero = 
  (unitSpray ^-^ x^**^2) ^*^ g''
  ^-^ (2.^alpha ^+^ unitSpray) *^ (x ^*^ g')
  ^+^ n.^(nAsSpray ^+^ 2.^alpha) *^ g
putStrLn $ prettySpray shouldBeZero
-- 0
```

Now, how to substitute a value to the parameter $\alpha$? The package provides 
the function `evalSpraySpray` to perform this task:

```haskell
putStrLn $ 
  prettyQSpray'' $ evalSpraySpray g [1]
-- 8*X^3 - 4*X
```

This is a `Spray Rational` spray.


## The `SymbolicSpray` type

If you have only one symbolic coefficient, you can deal with the sprays 
of type `SymbolicSpray a`. These are sprays whose coefficients are 
*ratios of univariate polynomials*, 
so this allows more possibilities than a `Spray (Spray a)`. Since the variable 
of these univariate polynomials occurs in the coefficients of such a spray, I 
call it the *outer variable* sometimes, although I do not very like this name 
(see below). And I say that the variables of the symbolic spray are the 
*inner variables* or the *main variables*, though I would prefer to simply call 
them the *variables*.
Assume you want to deal with the polynomial `4/5 * a/(a² + 1) * (x² + y²) + 2a/3 * yz`. 
Then you define it as follows:

```haskell
import           Prelude hiding ((*), (+), (-), (/), (^), (*>))
import qualified Prelude as P
import           Algebra.Additive              
import           Algebra.Module            
import           Algebra.Ring
import           Algebra.Field
import           Math.Algebra.Hspray
import           Number.Ratio       ( (%), T ( (:%) ) )
x = lone 1 :: SymbolicQSpray 
y = lone 2 :: SymbolicQSpray 
z = lone 3 :: SymbolicQSpray 
a = outerQVariable  
sSpray 
  = ((4%5) *. (a :% (a^2 + one))) *> (x^2 + y^2)  +  (constQPoly (2%3) * a) *> (y * z)
putStrLn $ prettySymbolicQSpray' "a" sSpray
-- { [ (4/5)*a ] %//% [ a^2 + 1 ] }*X^2 + { [ (4/5)*a ] %//% [ a^2 + 1 ] }*Y^2 + { (2/3)*a }*Y.Z
```

There are three possible evaluations of a symbolic spray:

```haskell
-- substitute a value for 'a':
putStrLn $ 
  prettyQSpray''' $ evalSymbolicSpray sSpray (6%5)
-- (24/61)*X^2 + (24/61)*Y^2 + (4/5)*Y.Z

-- substitute a value for 'a' and some values for 'X', 'Y', 'Z':
evalSymbolicSpray' sSpray (6%5) [2, 3, 4%7]
-- 13848 % 2135

-- substitute some values for 'X', 'Y', 'Z':
putStrLn $ 
  prettyRatioOfQPolynomials "a" $ evalSymbolicSpray'' sSpray [2, 3, 4%7]
-- [ (8/7)*a^3 + (404/35)*a ] %//% [ a^2 + 1 ]
```

Although it does not make sense to replace the main variables (`X`, `Y`, `Z`)
of a symbolic spray with some fractions of univariate polynomials, this feature 
is not provided. We rather consider that a `SymbolicSpray K` spray defines a 
multivariate polynomial on the field `K` whose coefficients lie in `K` but 
depend on a parameter, the so-called outer variable (`"a"`). By the way I am not 
a fan of this name, and maybe the *parameter* would be a better name? And then
*parametric spray* would be a better name than *symbolic spray*? Do not 
hesitate to open a Github issue to leave some comments if you want! 

The nice point regarding these ratios of univariate polynomials is that they 
are automatically "simplified" (i.e. written as irreducible fractions). 
For example:

```haskell
polyFrac = (a^8 - one) ^/^ (a - one)
putStrLn $ prettyRatioOfQPolynomials "a" polyFrac
-- a^7 + a^6 + a^5 + a^4 + a^3 + a^2 + a + 1
```

Maybe you prefer the fractional form, but it is nice to see that this ratio of 
polynomials actually is a polynomial. 
Note that I used `^/^` here and not `:%`. That's because `:%` does not simplify 
the fraction, it just constructs a fraction with the given numerator and denominator.
Whenever an arithmetic operation is performed on a fraction, the result is always 
simplified. So the `^/^` operator simply constructs a fraction with `:%` and then 
it multiplies it by one to get the simplification.


## As of version 0.2.7: `RatioOfSprays`

So far we have good stuff to deal with symbolic coefficients: the 
`Spray (Spray a)` sprays and the `SymbolicSpray a` sprays. The 
`SymbolicSpray a` sprays are successfully used in the 
[**jackpolynomials** package](https://github.com/stla/jackpolynomials). 
However this is not enough. For example we cannot implement the 
[Jacobi polynomials](https://en.wikipedia.org/wiki/Jacobi_polynomials) 
with symbolic parameters, because they have two parameters and their 
recurrence relation involves some divisions of their coefficients. 

We need a new type, similar to `SymbolicSpray a` but allowing multivariate 
fractions of polynomials for the coefficients.

A first step in this direction has been achieved in version 0.2.7: the 
type `RatioOfSprays a`, whose objects represent ratios of sprays, has 
been introduced. Thus it suffices to introduce the type 
`Spray (RatioOfSprays a)` now.

Thus the `Spray (RatioOfSprays a)` sprays are more general than the 
`SymbolicSpray a` sprays, which are restricted to univariate fractions 
of polynomials. But it is possible that the `Spray (RatioOfSprays a)` 
sprays will be less efficient than the `SymbolicSpray a` sprays in the 
univariate case. I will have to benchmark in order to get an answer to 
this question.

To construct a ratio of sprays, apply `%//%` between its numerator and 
its denominator:

```haskell
import Math.Algebra.Hspray
x = qlone 1 -- shortcut for  lone 1 :: Spray Rational
y = qlone 2 
rOS = (x ^-^ y) %//% (x^**^2 ^-^ y^**^2)
putStrLn $ prettyRatioOfQSprays rOS
-- [ 1 ] %//% [ x + y ]
```

The `%//%` operator always returns an irreducible fraction.

The `RatioOfSprays a` type makes sense when `a` has a field instance, and then 
it has a field instance too. To use the field operations, import the necessary
modules from **numeric-prelude**, and hide these operations from the `Prelude`
module (then you can also use the **numeric-prelude** operations for sprays, 
instead of using `^+^`, `^-^`, `^*^`, `^**^`):

```haskell
import Prelude hiding ((+), (-), (*), (/), (^), (*>), (<*))
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module
import Algebra.RightModule
import Algebra.Ring
import Algebra.Field              
import Math.Algebra.Hspray
x = qlone 1  
y = qlone 2 
p = x^2 - 3*^(x * y) + y^3 
q = x - y
rOS1 = p^2 %//% q
rOS2 = rOS1 + unitRatioOfSprays
rOS = rOS1^2 + rOS1*rOS2 - rOS1/rOS2 + rOS2 -- slow!
(rOS1 + rOS2) * (rOS1 - rOS2) == rOS1^2 - rOS2^2
-- True
```

The `RatioOfSprays a` type also has left and right module instances over `a` 
and over `Spray a` as well. That means you can multiply a ratio of sprays by
a scalar and by a spray, by using, depending on the side, either `*>` or `<*`:

```haskell
import Data.Ratio ( (%) )
rOS' = (3%4::Rational) *> rOS^2  +  p *> rOS
rOS' / rOS' == unitRatioOfSprays
-- True
```

You can also divide a ratio of sprays by a spray with `%/%`:

```haskell
p *> (rOS' %/% p) == rOS'
-- True
rOS1 %/% p == p %//% q
-- True
```

When `a` has a field instance, both a `Spray a` spray and a `RatioOfSprays a` 
ratio of sprays can be divided by a scalar with the `/>` operator:

```haskell
k = 3 :: Rational
(p /> k) *> rOS == p *> (rOS /> k)
-- True
```

Use `evalRatioOfSprays` to evaluate a ratio of sprays:

```haskell
import Data.Ratio ( (%) )
f :: Algebra.Field.C a => a -> a -> a
f u v = u^2 + u*v - u/v + v
rOS == f rOS1 rOS2
-- True
values = [2%3, 7%4]
r1 = evalRatioOfSprays rOS1 values
r2 = evalRatioOfSprays rOS2 values
evalRatioOfSprays rOS values == f r1 r2
-- True
```


## Other features

Resultant and subresultants of two polynomials, and greatest common divisor of 
two polynomials with coefficients in a field.