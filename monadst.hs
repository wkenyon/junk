import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe

data TwoPlayers = A | B deriving (Read, Show,Eq,Enum)

instance Enum a => (Enum (Maybe a)) where
     fromEnum Nothing = -1
     fromEnum (Just a) = fromEnum a
     toEnum (-1) = Nothing
     toEnum n = Just (toEnum n)
 
makeArray :: Board -> Int -> UArray (Int,Int) Int
makeArray board n = runSTUArray $ do
    newListArray ((1,1), (n,n)) (map fromEnum $ concat board)

checkArray :: (UArray (Int,Int) Int) -> Int -> (Int, Maybe TwoPlayers)
checkArray arr k = foldl checkCell (k, Nothing) (range $ bounds arr)
 where
  checkCell :: (Int, Maybe TwoPlayers) -> (Int, Int) -> (Int, Maybe TwoPlayers)
  checkCell (0, player) _ = (0, player)
  checkCell _ (a,1) = (k, toEnum (arr ! (a,1)))
  checkCell (n, player) index | (player == toEnum (arr ! index))
                                && (player /= Nothing) = (n-1, player)
                              | otherwise = (k, toEnum (arr ! index))

--doConnectk :: [(Int, Int)] -> (Int, Maybe TwoPlayers) -> UArray (Int,Int) (Maybe TwoPlayers 
--        (_,winner) <- foldForM [1..n] (k,Nothing) $ \(b::(Int,Maybe TwoPlayers) -> \y::Int -> do 
--                          player <- readArray board (x,y)
--                          (num::Int, player'::(Maybe TwoPlayers)) <- b
--                          if (num==0) then return (num,player)
--                          else if (player==player') then return (num-1,player)
--                          else return (k, player)
--        if isNothing a then return winner
--        else return a
        
