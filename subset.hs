main :: IO ()
main = do
  name <- getLine
  putStrLn name
  putStrLn $ show $ f 5

f :: Int -> Int
f 0 = 1
f 1 = 1
f x = y + z
  where y = f $ x - 1
        z = f $ x - 2

find :: Int -> [Int] -> Bool
find x xs = (x == head xs) || (find x (tail xs))

subset :: [Int] -> [Int] -> Bool
subset [] ys = True
subset xs ys = (find (head xs) ys) && (subset (tail xs) ys)
