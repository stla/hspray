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

* New function `sprayDivision`, to perform the division of a spray by a list of sprays.

* New function `groebner`, to compute a Groebner basis of a list of sprays.

* New function `isSymmetricSpray`, to check whether a spray is a symmetric polynomial.

* New function `isPolynomialOf`, to check whether a spray can be expressed as a polynomial of a given list of sprays.


## 0.2.1.0 - 2024-03-22

* New functions `permuteVariables` and `swapVariables`.

* New function `resultant`, to compute the resultant of two sprays.

* New function `subresultants`, to compute the subresultants of two sprays.