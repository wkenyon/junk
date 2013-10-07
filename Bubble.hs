import Data.Maybe
import Debug.Trace

bubblesort :: Ord a => [a]->[a]
bubblesort xs = traceStack "hey" $let xs' = doFstSwp xs in case xs' of 
                                                                Just xs'' -> bubblesort xs''
                                                                Nothing -> xs
doFstSwp :: Ord a => [a] -> Maybe [a]
doFstSwp [] = Nothing
doFstSwp [x] = Nothing
doFstSwp (x1:x2:xs) | (x2 < x1) = Just $ x2:x1:xs
                    | otherwise =  x1 `justCons` doFstSwp(x2:xs)

justCons :: a -> Maybe [a] -> Maybe [a]
justCons _ Nothing = Nothing
justCons x1 (Just xs) = Just (x1:xs)



