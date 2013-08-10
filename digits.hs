import Data.Char
checkUtil s = 
  let ss = filter isDigit s in 
     let sets = zipWith (\x y -> x* (read y::Int)) [1..9] (map (\x -> [x]) (init ss)) in
        mod (sum sets) 11
--        let tt = mod (sum sets) 11 in
--           show tt == (map (\x -> [x]) last ss)
check s = show (checkUtil s) == (last (filter isDigit s)):""

main1 s
  | check s = "Right"
  | otherwise = init s ++ show (checkUtil s)

main = do
  s <- getLine
  putStrLn $ main1 s
  main
