# Changelog for `hspray`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).


## 0.1.0.0 - 2022-12-11

First release.


## 0.1.1.0 - 2022-12-12

* New functions `toList`, `sprayTerms` and `bombieriSpray`.

* New operation `.^`, to multiply a spray by an integer.

* Added some unit tests.


## 0.1.2.0 - 2023-02-24

New function `derivSpray`, to differentiate a spray.


## 0.1.3.0 - 2023-08-29

* `Powers(..)` is now exported.

* Completed the README to show how to deal with symbolic coefficients.


## 0.2.0.0 - 2024-03-14

* New functions `prettySpray'` and `prettySprayXYZ`.

* New function `substituteSpray`.

* New function `sprayDivision`, to perform the division of a spray by a list 
of sprays.

* New function `groebner`, to compute a Gröbner basis of a list of sprays.

* New function `isSymmetricSpray`, to check whether a spray is a symmetric 
polynomial.

* New function `isPolynomialOf`, to check whether a spray can be expressed as 
a polynomial of a given list of sprays.


## 0.2.1.0 - 2024-03-22

* New functions `permuteVariables` and `swapVariables`.

* New function `resultant`, to compute the resultant of two sprays.

* New function `subresultants`, to compute the subresultants of two sprays.


## 0.2.1.1 - 2024-03-25

* Improved the documentation.

* Flipped the order of appearance of the terms in the output of the 
`prettySpray` functions.


## 0.2.2.0 - 2024-03-26

* Fixed an error in `esPolynomial`, which resulted to a bug in 
`isSymmetricSpray`.


## 0.2.3.0 - 2024-03-28

* New unit tests.

* Fixed `resultant` and `subresultants`: the variables of the sprays they 
return were incorrect.

* New function `gcdQX`, to compute the greatest common divisor of two 
univariate sprays with rational coefficients.


## 0.2.4.0 - 2024-03-30

* Flipped the order of the arguments in `permuteVariables` and `swapVariables`.

* New function `gcdSpray`, to compute the greatest common divisor of two sprays
with coefficients in a field. 

* The function `gcdQX` has been removed since `gcdSpray` is more general.

* The function `sprayDivision` has been renamed to `sprayDivisionRemainder`.

* New function `sprayDivision`, returning the quotient and the remainder of the 
division of two sprays.


## 0.2.5.0 - 2024-04-02

* New function `resultant'` which computes the resultant of two sprays with 
coefficients in a field. Thus it is less general than the function `resultant` 
but it is more efficient. 

* Fixed a small mistake in `isSymmetricSpray` and `isPolynomialOf`: these 
functions didn't deal with the constant term of the spray.

* New function `psPolynomial` which computes the power sum polynomials.

* A particular type of sprays, namely `SymbolicSpray a`, has been introduced. 
The coefficients of these sprays are ratios of univariate polynomials with `a` 
coefficients. There is a specialization `SymbolicQSpray` for the case when `a` 
is a type of rational numbers. The necessary instances have been defined and 
there is the function `prettySymbolic(Q)Spray` to display such sprays. There are 
also some functions to perform evaluation of such sprays.


## 0.2.6.0 - 2024-04-15

* New function `collinearSprays` which checks whether two sprays are collinear.

* The function `isPolynomialOf` threw an error when the number of variables in
the spray to be tested was less than the number of variables in the list of 
sprays. That is me who programmed this error and this was wrong: for example, 
`x = p1 - p2^*^p3` with `p1 = x + y^*^z`, `p2 = y`, and `p3 = z`.

* New functions to print sprays with numeric coefficients, such as 
`prettyNumSpray` and `prettyQSpray`.

* The functions `prettySpray`, `prettySpray'` and `prettySpray''` have been 
changed.

* New functions to print symbolic sprays.

* Documentation and README have been improved.


## 0.2.7.0 - 2024-04-19

* Defined `qlone`, which is the same as `lone` but always returns a rational 
spray (a `QSpray` spray).

* The function `sprayDivision` ran into an infinite loop when the divisor was
constant. This has been fixed.

* New function `characteristicPolynomial`, to compute the characteristic 
polynomial of a matrix. 

* Gegenbauer polynomials. They have been implemented mainly to provide an 
illustration of the type `Spray (Spray a)` in README.

* New function `evalSpraySpray`, to evaluate the spray coefficients of a 
`Spray (Spray a)` spray, thereby yielding a `Spray a` spray.

* New type `RatioOfSprays a`, whose objects represent ratios of polynomials 
represented by two `Spray a` objects. Thus the type `Spray (RatioOfSprays a)` 
allows more possibilities than the type `SymbolicSpray a` because it is not 
restricted to univariate fractions of polynomials, and obviously it also 
allows more possibilities than the type `Spray (Spray a)`. Instances and 
arithmetic operations for these ratios of sprays have been defined. The result 
of an arithmetic operation always is an irreducible fraction. See README for 
examples.

* Jacobi polynomials. They have been implemented mainly to experiment the type 
`Spray (RatioOfSprays a)`. By the way, this type has been named 
`ParametricSpray a`, but this is possibly temporary.

* The class `HasVariables a` has been introduced in order to have some functions
which apply to both `Spray a` objects and `RatioOfSprays a` objects.

* The function `derivSpray` no longer exists. To get a derivative of a spray, 
use the `derivative` function, which is also applicable to a ratio of sprays 
(this is a method of the class `HasVariables`).

* A spray with coefficients in a field can now be divided by a scalar by using
the `/>` operator. This operator can also be used to divide a ratio of sprays 
(with coefficients in a field) by a scalar.


## 0.3.0.0 - 2024-04-21

* The type `SymbolicSpray a` has been renamed to `OneParameterSpray a`, and 
all functions names which contained the string `Symbolic` have been changed by
replacing `Symbolic` with `OnePerameter`.

* The class `HasVariables`, which is instantiated for `Spray` and 
`RatioOfSprays`, has a new method `changeVariables` allowing to perform 
polynomial transformations of the variables of a spray and of a ratio of 
sprays. For sprays, this is the same as the `composeSpray` function.

* The class `HasVariables` is now also instantiated for `RatioOfPolynomials`.

* The type alias `ParametricSpray a = Spray (RatioOfSprays a)` has been kept 
and the type alias `SimpleParametricSpray a = Spray (Spray a)` has been 
introduced. We say that a `Spray b` spray is parametric when `b` has the 
`HasVariables` instance. So this applies to a `ParametricSpray a` spray, to 
a `SimpleParametricSpray a` spray, and also to a `OneParameterSpray a` spray
(recall that `OneParameterSpray a = Spray (RatioOfPolynomials a)`).

* Functions to print `ParametricSpray` sprays and `SimpleParametricSpray` 
sprays.

* Function `numberOfParameters`, returning the number of parameters of a 
parametric spray, that is to say the number of variables occurring in the 
coefficients of this spray.

* Function `changeParameters`, to perform polynomial transformations of the 
parameters of a parametric spray.

* Function `substituteParameters` to replace the parameters of a parametric 
spray with some values. This is the same as `evalSpraySpray`(which will 
probably disappear in the future).

* Function `evalParametricSpray` to replace the variables of a parametric 
spray with some values.


## 0.3.1.0 - 2024-XX-XX

* Slight improvement of the `jacobiPolynomial` function.

* Function `lone'` to construct monomials like `x_n^p` more efficiently than 
`lone n ^**^ p`.

* Function `monomial` to construct monomials like `x_1^4.x_3^7`.


