import System.Environment
import Control.Monad.ST
import Control.Monad
import Data.STRef

--sumST :: Num a => [a] -> a
--sumST xs = runST $ \x->do
--  n <- newSTRef 0
--  forM_ xs $ \x-> modifySTRef n (+x)
--  readSTRef n

f :: Int -> Int
f 0 = 1
f 1 = 1
f x = y + z
  where y = f $ x - 1
        z = f $ x - 2

choose' :: [[b]] -> [[b]]
choose' [] = [[]]
choose' (ms:mss) = do
  a <- choose' mss
  map (:a) ms


--blah :: IO ()
--blah = do
--  x <- System.Environment.getArgs
--  seq x $ putStrLn "hellooooo"
--  seq x $ mapM_ putStrLn x
--  seq x $ y <- System.Environment.getProgName
--  seq x $ putStrLn y
