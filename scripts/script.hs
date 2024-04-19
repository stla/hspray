import Data.Matrix (Matrix, fromLists)
import Math.Algebra.Hspray
m = fromLists [ [12, 16, 4]
              , [16, 2, 8]
              , [8, 18, 10] ] :: Matrix Int
spray = characteristicPolynomial m
