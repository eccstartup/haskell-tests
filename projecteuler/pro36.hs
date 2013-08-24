import Numeric
import Data.Char

toBase2 n = showIntAtBase 2 intToDigit n ""

palindrome n = show n == m
           where m = reverse $ show n

palindrome2 s = s == reverse s

doublePa n = (palindrome n) && (palindrome2 $ toBase2 n)

countDou = foldr (+) 0 $ filter doublePa [1..1000000]
