import Data.List
import Data.Maybe

--buils a string with all charactars
build1 s = nub $ s ++ ['A'..'Z']

--build a list then transpose
build2 s n
  | length s <= n = (s ++ map (\x -> '0') [1..n - length s]) : []
  | otherwise = take n s : build2 (drop n s) n

--build the cipher string
build3 ss = filter (/='0') xs
  where xs = concat $ sort $ transpose ss

cps s = build3 $ build2 (build1 s) (length $ nub s)
        
decode1 b s
  | b == ' ' = ' '
  | otherwise = ['A'..'Z'] !! n where
    n = fromJust $ elemIndex b (cps s)

encode1 b s
  | b == ' ' = ' '
  | otherwise = (cps s) !! n where
    n = fromJust $ elemIndex b ['A'..'Z']

encodeMessage s m = map (\x -> encode1 x s) m

decodeMessage s m = map (\x -> decode1 x s) m

sayMe [] = do
  return ()
sayMe (a:b:xs)= do
  putStrLn $ map (\s -> decode1 s a) b
  sayMe xs

main = do
  lists <- getContents
  let lins = tail $ lines lists
  sayMe lins
