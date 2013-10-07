append :: [a]->[a]->[a]
append [] ys = ys
append (x:xs) ys = x:(append xs ys)

main = putStrLn $ show $ append [1,2,3] [4,5,6]
