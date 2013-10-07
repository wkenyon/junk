fib 0 = []
fib 1 = [1]
fib 2 = [1,1]
fib n = ((head $ fib (n-1)) + (head $ fib (n-2))) : (fib (n-1))
