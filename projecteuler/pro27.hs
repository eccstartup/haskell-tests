import Data.Numbers.Primes

countPr a b = length $ takeWhile isPrime [n^2+a*n+b| n<-[0..]]

findMaxPr = foldr (\(a,b) (c,d) -> if b>d then (a,b) else (c,d)) (0,0) [(a*b,countPr a b) | a<-[-999..999], b<-[-999..999]]
