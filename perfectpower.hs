import Data.Numbers.Primes

perfect n = 
  let s = primeFactors n
    in let a = length s > 1 
      in let b = head s == last s
        in a && b