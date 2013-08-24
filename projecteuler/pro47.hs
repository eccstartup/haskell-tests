import Data.Numbers.Primes
import Data.List

haveFour n = (length . group $ primeFactors n) == 4

conFour n = all haveFour [n..n+3]

firstFour = head $ filter conFour [210..]
