--cat words.txt | tr '\",\"' "\n" >word1.txt
--and you will get a different format of words.txt

import Data.Maybe
import Data.List

triNums = take 100 [div (n*(n+1)) 2 | n<-[1..]]

triNum n = elem n triNums

toNum c = (fromJust $ elemIndex c ['A'..'Z']) + 1

isTri s = triNum $ sum $ map toNum s


main = do
  lists <- getContents
  let lis = lines lists
  putStrLn $ show . length $ filter isTri lis
