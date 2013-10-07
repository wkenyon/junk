main = print (take 10 fibs)
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)