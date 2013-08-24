
triNums = take 100000 [div (n*(n+1)) 2 | n<-[1..]]

--triNum n = elem n triNums

penNums = take 100000 [div (n*(3*n-1)) 2 | n<-[1..]]

--penNum n = elem n penNums

hexNums = take 100000 [n*(2*n-1) | n<-[1..]]

--hexNum n = elem n hexNums

--allNum n = (triNum n) && (penNum n) && (hexNum n)


isTri m = m2 == n*(n+1)
          where m2 = 2*m
                n = round $ sqrt $ fromInteger m2
                
isPen m = m2 == n*(3*n-1)
          where m2 = 2*m
                n = round $ sqrt $ (fromInteger m2) / 3

isHex m = m == n*(2*n-1)
          where n = round $ sqrt $ (fromInteger m) / 2
                
allNum1 n = (isTri n) && (isPen n) && (isHex n)

solNum = head $ filter allNum1 $ drop 285 triNums