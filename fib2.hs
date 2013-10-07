fib1 0 = error "argh" 
fib1 1 = 1
fib1 n = (fib1 (n-1)) + (fib1 (n-2))

fib2 n = fibs !! n
  where fibs = [1,1] ++ map' (\x->(fibs!!x + fibs!!(x-1))) [1..]

main = do let x = map' (fib2 . fib1) [1..10]  

          putStrLn (show x)
          

map' _ [] = []
map' f (x:xs) = f x :  map' f xs
