import Control.Monad.List

dropf  :: [[a]] -> [a]
dropf xss = do
            xs <- xss
            return $ head xs


dropf' :: [[a]] -> [a]
dropf' xss = xss >>= f
  where
    f xs = return $ head xs

f1 xss = (concat . map f2) xss 
f2 xs  = [head xs]

f xs ys = (head xs, head ys)
g = zipWith f [[1,2],[1,2]] [[1,2],[1,2]]

pairUp xs ys = do 
  x<-xs
  unless (x < head ys) []
  y<-ys
  unless (y < head xs) []
  return (x,y)
pairUp' xs ys = xs >>= f
  where
    f x = (unless (x < head ys) []) >>
          ys >>= g
            where
              g y = return (x,y)

runthis xs ys = [(x,y)|x<-xs,x<head ys,y<-ys]