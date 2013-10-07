import Prelude hiding (sum)

main = print $ sum [1..1000000] 0
sum [] n     = n
sum (x:xs) n = sum xs (n+x)