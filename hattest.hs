list :: [Int]
list = [1,2,0]

swap :: (a->b->c) -> b -> a -> c
swap f x y = f y x
main = putStrLn (show (last (map' (swap div) list) 1))
map' :: (a->b) -> [a] -> [b]
map' f xs = reverse $ foldl (\x -> \y -> (f y):x) [] xs
{-
I'd like this to produce a stack like:

main
map
map
map
div
error

-}