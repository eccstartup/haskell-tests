import Data.Numbers.Primes
import Data.List

belowMi = takeWhile (<1000000) primes

numAll = 78498

howManyis n xs = isPrime $ sum $ take n xs

startCount xs = length $ takeWhile (\n -> isPrime $ sum $ take n xs) [1..]
