import System.Random

main = do 
	gen <- getStdGen
	let (computer,_) = randomR (1,10) gen :: (Int, StdGen)
	putStrLn "What number between 1 and 10 am I thinking of?"
	user <- getLine
	if (computer == read user) then do
		putStrLn "Well done you got it right"
	else do
		putStrLn ("Oops that was wrong, it was " ++ (show computer))
	
