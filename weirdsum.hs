add = (+)

composes :: [a->a] -> a -> a
composes = foldl' (.) id

weirdSum :: [Int] -> Int
weirdSum xs = composes (map' add xs) 0

foldl' f z []     = z
foldl' f z (x:xs) = foldl' f (f z x) xs

map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)


{-
weirdSum 1:2:error
----------------


weirdSum 1:2:error
composes (map' add [1,2]) 0
foldl' (.) id (map' add [1,2]) 0
foldl' (.) id (add 1 : map' add [2]) 0
foldl' (.) id (add 1 : add 2 : map' add []) 0
foldl' (.) id (add 1 : add 2 : []) 0
foldl' (.) id (add 1 : add 2 : []) 0
foldl' (.) (id . add 1) (add 2 : []) 0
foldl' (.) (id . add 1 . add 2) [] 0
(id . add 1 . add 2) 0
-}