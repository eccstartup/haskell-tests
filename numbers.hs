import Data.List

encd s = sum $ map ( \x -> (length x)^2 * (read ((head x):"")::Int) ) (group (show s))

rangeEncd a b = foldr (+) 0 (map encd [a..b])

main = do
  s <- getLine
  putStrLn $ show $ let l = words s in
    rangeEncd (read (l !! 0) :: Int) (read (l !! 1) :: Int)
