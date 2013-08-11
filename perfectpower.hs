import Data.Numbers.Primes
import Data.List

perfect n = 
  let s = helper1 (primeFactors n)
    in let a = head s > 1 
      in let b = head s == last s
        in a && b
           
helper1 l = sort (map length (group l))