import Data.Numbers.Primes
import Data.List

perfect n = helper2 n > 1
           
helper1 l = sort $ map length (group l)

helper2 l = 
  let s = helper1 $ primeFactors l 
    in foldr gcd (head s) s