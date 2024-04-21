# hspray

<!-- badges: start -->
[![Stack-lts](https://github.com/stla/hspray/actions/workflows/Stack-lts.yml/badge.svg)](https://github.com/stla/hspray/actions/workflows/Stack-lts.yml)
[![Stack-nightly](https://github.com/stla/hspray/actions/workflows/Stack-nightly.yml/badge.svg)](https://github.com/stla/hspray/actions/workflows/Stack-nightly.yml)
<!-- badges: end -->

***Simple multivariate polynomials in Haskell.*** 
This package deals with multivariate polynomials over a commutative ring, 
fractions of multivariate polynomials over a commutative field, and 
multivariate polynomials with symbolic parameters in their coefficients.

____

The main type provided by this package is `Spray a`. 
An object of type `Spray a` represents a multivariate polynomial whose
coefficients are represented by the objects of type `a`. For example:

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
variables with the `lone` function, and then combine them with arithmetic 
operations.

There are numerous functions to print a spray. If you don't like the letters 
`x`, `y`, `z` in the output of `prettyNumSpray`, you can use `prettyNumSprayXYZ` 
to change them to whatever you want:

```haskell
putStrLn $ prettyNumSprayXYZ ["A","B","C"] poly
-- 8.0*A^4.B^2.C^2 + 4.0*A^3.B.C
```

Note that this function does not throw an error if you don't provide enough 
letters; in such a situation, it takes the first given letter and it appends 
it with the digit `i` to denote the `i`-th variable: 

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

We will come back to these sprays of type `Spray (Spray a)`. They can 
be used to represent parametric polynomials.


#### Evaluation of a spray:

```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
spray = 2 *^ (x ^*^ y ^*^ z) 
-- evaluate spray at x=2, y=1, z=2
evalSpray spray [2, 1, 2]
-- 8.0
```

#### Partial evaluation:

```haskell
import Math.Algebra.Hspray
import Data.Ratio
x1 = lone 1 :: Spray Rational
x2 = lone 2 :: Spray Rational
x3 = lone 3 :: Spray Rational
spray = x1^**^2 ^+^ x2 ^+^ x3 ^-^ unitSpray
putStrLn $ prettyQSprayX1X2X3 "x" spray
-- x1^2 + x2 + x3 - 1
--
-- substitute x1 -> 2 and x3 -> 3, and don't substitute x2
spray' = substituteSpray [Just 2, Nothing, Just 3] spray
putStrLn $ prettyQSprayX1X2X3 "x" spray'
-- x2 + 6
```

#### Differentiation of a spray:

```haskell
import Math.Algebra.Hspray
x = lone 1 :: Spray Double
y = lone 2 :: Spray Double
z = lone 3 :: Spray Double
spray = 2 *^ (x ^*^ y ^*^ z) ^+^ (3 *^ x^**^2)
putStrLn $ prettyNumSpray spray
-- 3.0*x^2 + 2.0*x.y.z
--
-- derivative with respect to x
putStrLn $ prettyNumSpray $ derivative 1 spray
-- 6.0*x + 2.0*y.z"
```

## Gröbner bases

As of version 2.0.0, it is possible to compute a Gröbner basis of the ideal 
generated by a list of spray polynomials.

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
spray  = ((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^-^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
spray' = ((2%3) *^ (x^3 * y * z) - x^2) * ((7%4) *^ (x * y * z))
spray == spray'
-- True
```

Note that `*>` could be used instead of `*^` but running `lambda *> spray` 
possibly throws an "ambiguous type" error regarding the type of `lambda`.

Maybe better (I didn't try yet), follow the "Usage" section on the 
[Hackage page](https://hackage.haskell.org/package/numeric-prelude-0.4.4#usage) 
of **numeric-prelude**.


## Symbolic parameters in the coefficients

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

spray = a *^ (x^2 + y^2) + ((2 *^ b) /^ 3) *^ z 
putStrLn $ 
  showSprayXYZ' (prettyQSprayXYZ ["a","b"]) ["X","Y","Z"] spray
-- (a)*X^2 + (a)*Y^2 + ((2/3)*b)*Z
```

You can extract the powers and the coefficients as follows:

```haskell
l = toList spray
map fst l
-- [[0,0,1],[2],[0,2]]
map toList $ map snd l
-- [[([0,1],2 % 3)],[([1],1 % 1)],[([1],1 % 1)]]
```

These `Spray (Spray a)` sprays can be very useful. They represent polynomials 
whose coefficients polynomially depend on some parameters. 
Actually there is a type alias of `Spray (Spray a)` in **hspray**, namely 
`SimpleParametricSpray a`, and there are some convenient functions to deal 
with sprays of this type. There is also a type alias of 
`SimpleParametricSpray Rational`, namely `SimpleParametricQSpray`.
For example we can print our `SimpleParametricQSpray` spray `spray` as follows:

```haskell
putStrLn $ 
  prettySimpleParametricQSprayABCXYZ ["a","b"] ["X","Y","Z"] spray
-- { a }*X^2 + { a }*Y^2 + { (2/3)*b }*Z
```

The 
[Gegenbauer polynomials](https://en.wikipedia.org/wiki/Gegenbauer_polynomials)
are a real-life example of polynomials that can be represented by 
`SimpleParametricQSpray` sprays. They are univariate polynomials whose 
coefficients polynomially depend on a parameter $\alpha$ (the polynomial 
dependency is clearly visible from the recurrence relation given on 
Wikipedia). Here is their recursive implementation in **hspray**:

```haskell
gegenbauerPolynomial :: Int -> SimpleParametricQSpray 
gegenbauerPolynomial n 
  | n == 0 = unitSpray
  | n == 1 = (2.^a) *^ x
  | otherwise = 
    (2.^(n'' ^+^ a) /^ n') *^ (x ^*^ gegenbauerPolynomial (n - 1))
    ^-^ ((n'' ^+^ 2.^a ^-^ unitSpray) /^ n') *^ gegenbauerPolynomial (n - 2)
  where 
    x = lone 1 :: SimpleParametricQSpray
    a = lone 1 :: QSpray
    n'  = toRational n
    n'' = constantSpray (n' - 1)
```

Let's try it:

```haskell
n = 3
g = gegenbauerPolynomial n
putStrLn $ 
  prettySimpleParametricQSprayABCXYZ ["alpha"] ["X"]  g
-- { (4/3)*alpha^3 + 4*alpha^2 + (8/3)*alpha }*X^3 + { -2*alpha^2 - 2*alpha }*X
```

Let's check the differential equation given in the Wikipedia article:

```haskell
g'  = derivative 1 g
g'' = derivative 1 g'
alpha = lone 1 :: QSpray
x     = lone 1 :: SimpleParametricQSpray
nAsSpray = constantSpray (toRational n)
shouldBeZero = 
  (unitSpray ^-^ x^**^2) ^*^ g''
    ^-^ (2.^alpha ^+^ unitSpray) *^ (x ^*^ g')
      ^+^ n.^(nAsSpray ^+^ 2.^alpha) *^ g
putStrLn $ prettySpray shouldBeZero
-- 0
```

Now, how to substitute a value to the parameter $\alpha$? For example, it is 
said in the Wikipedia article that this yields the Legendre polynomials for 
$\alpha = 1/2$. The package provides the function `substituteParameters` to 
perform this task:

```haskell
import Data.Ratio (%)
putStrLn $ 
  prettyQSpray'' $ substituteParameters g [1%2]
-- (5/2)*X^3 - (3/2)*X
```

This is a `Spray Rational` spray.

The Wikipedia article also provides the value at $1$ of the Gegenbauer 
polynomials in function of $\alpha$. We can get this value with 
`evalParametricSpray`:

```haskell
putStrLn $ 
  prettyQSprayXYZ ["alpha"] $ evalParametricSpray g [1]
-- (4/3)*alpha^3 + 2*alpha^2 + (2/3)*alpha
```

This is also a `Spray Rational` spray.


## Ratios of sprays and general parametric sprays

Since you have just seen that the type `Spray (Spray a)` is named 
`SimpleParametricSpray`, you probably guessed there is also a more general 
type named `ParametricSpray`. Yes, and this is an alias of 
`Spray (RatioOfSprays a)`, where the type `RatioOfSprays a` has not been 
discussed yet. The objects of this type represent fractions of multivariate 
polynomials and so this type is a considerable enlargment of the `Spray a` 
type. Thus the `Spray (RatioOfSprays a)` sprays can represent multivariate 
polynomials whose coefficients depend on some parameters, with a dependence 
described by a fraction of polynomials in these parameters. Let's start with 
a short presentation of the ratios of sprays.

### The `RatioOfSprays` type

The type `RatioOfSprays a`, whose objects represent ratios of sprays, has 
been introduced in version 0.2.7.0. 
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

The `%//%` operator always returns an *irreducible fraction*. If you are 
***sure*** that your numerator and your denominator are coprime, you can use
the `%:%` instead, to gain some efficiency. But if they are not coprime, this 
can have unfortunate consequences.

The `RatioOfSprays a` type makes sense when `a` has a field instance, and then 
it has a field instance too. To use the field operations, import the necessary
modules from **numeric-prelude**, and hide these operations from the `Prelude`
module; then you can also use the **numeric-prelude** operations for sprays, 
instead of using `^+^`, `^-^`, `^*^`, `^**^`:

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
rOS / rOS == unitRatioOfSprays
-- True
```

The `RatioOfSprays a` type also has left and right module instances over `a` 
and over `Spray a` as well. That means you can multiply a ratio of sprays by
a scalar and by a spray, by using, depending on the side, either `*>` or `<*`:

```haskell
import Data.Ratio ( (%) )
rOS' = (3%4::Rational) *> rOS^2  +  p *> rOS
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

### The `ParametricSpray` type

Recall that `SimpleParametricSpray a = Spray (Spray a)` and 
`ParametricSpray a = Spray (RatioOfSprays a)`, and we have the aliases 
`SimpleParametricQSpray = SimpleParametricSpray Rational` and 
`ParametricQSpray = ParametricSpray Rational`.

The functions `substituteParameters` and `evalParametricSpray`, that we 
previously applied to a `SimpleParametricSpray a` spray, are also applicable 
to a `ParametricSpray a` spray. We didn't mention the function 
`changeParameters` yet, which is also applicable to these two types of sprays.
This function performs some polynomial transformations of the parameters of a
parametric spray. 
For example, consider the 
[Jacobi polynomials](https://en.wikipedia.org/wiki/Jacobi_polynomials). 
They are univariate polynomials with two parameters $\alpha$ and $\beta$. 
They are implemented in **hspray** as `ParametricQSpray` sprays. In fact 
it seems that the coefficients of the Jacobi polynomials *polynomially* 
depend on $\alpha$ and $\beta$, and if this is true one could implement them 
as `SimpleParametricQSpray` sprays. I will come back to this point later. The 
recurrence relation defining the Jacobi polynomials involves a division which 
makes the type `ParametricQSpray` necessary anyway. 
The `changeParameters` function is useful to derive the Gegenbauer polynomials 
from the Jacobi polynomials. Indeed, as asserted in the Wikipedia article, 
the Gegenbauer polynomials coincide, up to a factor, with the Jacobi 
polynomials with parameters $\alpha - 1/2$ and $\alpha - 1/2$. Here is how 
to apply the `changeParameters` function to get this special case of Jacobi 
polynomials:

```haskell
import Data.Ratio ( (%) )
j = jacobiPolynomial 3
alpha = qlone 1
alpha' = alpha ^-^ constantSpray (1%2)
j' = changeParameters j [alpha', alpha']
```

Now let's come back to the conjecture claiming that the coefficients of the 
Jacobi polynomials *polynomially* depend on $\alpha$ and $\beta$, and thus 
these polynomials can be represented by `SimpleParametricQSpray` sprays. 
Maybe this can be deduced from a formula given in the Wikipedia article, I 
didn't spend some time on this problem. I made this conjecture because I 
observed this fact for some small values of $n$, and I tried the function
`canCoerceToSimpleParametricSpray` for other values, which always returned 
`True`. One can apply the function `asSimpleParametricSpray` to perform the 
coercion.


## The `OneParameterSpray` type

Finally, let us mention the `OneParameterSpray a` type. Objects of this type 
represent multivariate polynomials whose coefficients are fractions 
of polynomials in only one parameter. So they are less general than the 
`ParametricSpray a` sprays, but they are a bit more efficient.

Assume for example that you want to deal with the polynomial 
`4/5 * a/(a² + 1) * (x² + y²) + 2a/3 * yz`. 
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
x = lone 1 :: OneParameterQSpray 
y = lone 2 :: OneParameterQSpray 
z = lone 3 :: OneParameterQSpray
a = qsoleParameter
spray 
  = ((4%5) *. (a :% (a^2 + one))) *> (x^2 + y^2)  +  (constQPoly (2%3) * a) *> (y * z)
putStrLn $ prettyOneParameterQSpray' "a" spray
-- { [ (4/5)*a ] %//% [ a^2 + 1 ] }*X^2 + { [ (4/5)*a ] %//% [ a^2 + 1 ] }*Y^2 + { (2/3)*a }*Y.Z
```

Not very easy... If you are more comfortable with the `ParametricSpray` sprays, 
construct such a spray and convert it to a `OneParameterSpray` with the 
function `parametricSprayToOneParameterSpray` or 
`parametricQSprayToOneParameterQSpray`.

The functions we have seen for the simple parametric sprays and the parametric 
sprays are also applicable to the one-parameter sprays. These are sprays of 
type `Spray (RatioOfPolynomials a)`, where the type `RatioOfPolynomials a` 
deals with objects that represent fractions of *univariate* polynomials.

Similary to the ratios of sprays, the nice point regarding these ratios of 
univariate polynomials is that they are automatically written as irreducible 
fractions. For example:

```haskell
polyFrac = (a^8 - one) % (a - one)
putStrLn $ prettyRatioOfQPolynomials "a" polyFrac
-- a^7 + a^6 + a^5 + a^4 + a^3 + a^2 + a + 1
```

Note that I used `%` here and not `:%`. That's because `:%` does not reduce 
the fraction, it just constructs a fraction with the given numerator and 
denominator. Whenever an arithmetic operation is performed on a fraction, the 
result is always an irreducible fraction. 

The `OneParameterSpray a` sprays are used in the 
[**jackpolynomials** package](https://github.com/stla/jackpolynomials). 

There is a slightly annoying point to note: the type `OneParameterQSpray` 
is *not* `OneParameterSpray Rational`: it is `OneParameterSpray Rational'`, 
where `Rational'` is a type similar to `Rational` defined in the 
**numeric-prelude** package. I had to use this type because `Rational'` has 
the necessary instances. 


## Other features

Resultant and subresultants of two polynomials, and greatest common divisor of 
two polynomials with coefficients in a field.