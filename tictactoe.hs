{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Set as Set
import System.Random
import Data.List
import Text.PrettyPrint
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Data.List
import qualified Debug.Trace as Trace
import Maybe
import System( getArgs )
main = do 
	args <- getArgs
        masterGen <- newStdGen
	let (Connectk _ board _) = selectBestMove $ iterativeMcts (expand $ gameToLeaf $ Connectk 4 (read $ head args) playerList) 2000 masterGen
            (x, y) = cmpBoard 0 board $ read $ head args
              in print $ (show x) ++ " " ++ (show y)

cmpBoard :: Int -> Board -> Board -> (Int, Int)
cmpBoard _ [] _ = undefined
cmpBoard _ _ [] = undefined
cmpBoard n (x:xs) (y:ys) | x==y = cmpBoard (n+1) xs ys
                         | otherwise = (n, cmpRow 0 x y)
cmpRow :: Int -> [Maybe TwoPlayers] -> [Maybe TwoPlayers] -> Int
cmpRow _ [] _ = undefined
cmpRow _ _ [] = undefined
cmpRow n (x:xs) (y:ys) | x==y = cmpRow (n+1) xs ys
                         | otherwise = n
runTests = $quickCheckAll

--UTILITIES MOVE TO DIFFERENT FILE
logi :: Int -> Int
logi a = ceiling $ log $ (fromIntegral a)+1

--NOT REALLY SURE WHERE THIS SHOULD GO:
playTicTacToe::()
playTicTacToe=() 

doHumanTicTacToeMove :: TicTacToe -> Int -> TicTacToe
doHumanTicTacToeMove (TicTacToe x y z) n = TicTacToe (Set.delete n x) y (Set.insert n z)

class Game a where
	legalChildren :: a->[a]
	currentState :: a->GameState
	doPseudoRandomMove :: (RandomGen g) =>g -> a -> (a,g)

--The following is all Connectk stuff
type Board = [[Maybe TwoPlayers]]
type Turns = [TwoPlayers]
data Connectk = Connectk Int Board Turns deriving Eq

instance Show Connectk where
         show (Connectk k board turns) = (show k) ++ " " ++ (show board) ++ " " ++ (show $ take 10 turns)

instance Game Connectk where
        legalChildren (Connectk k board []) = undefined
        legalChildren (Connectk k board (p:ps)) = map (\a -> Connectk k a ps) (boardFillAllBlanks board)
          where
            boardFillAllBlanks :: Board -> [Board]
            boardFillAllBlanks [] = []
            boardFillAllBlanks (a:xs) = (map (:xs) (lineFillAllBlanks a)) ++ (map (a:)(boardFillAllBlanks xs))
            --append is not inneficient here because we're only preappending the number of elements on the row each time. 
            lineFillAllBlanks :: [Maybe TwoPlayers] -> [[Maybe TwoPlayers]]
            lineFillAllBlanks [] = [] 
            lineFillAllBlanks (a:xs) = case a of Nothing -> ((Just p):xs) : map (a:) (lineFillAllBlanks xs)
                                                 Just _  -> map (a:) (lineFillAllBlanks xs)

        currentState s = case winner of (Just x) -> Win x
                                        Nothing -> if (null $ legalChildren s) then Stale else InProgress

          where winner = firstMatch $ map kInARow [board, transpose $ kDiag $ board, transpose $ kDiag $ map reverse board, transpose board, transpose $ kDiag $ transpose $ board,  transpose $ kDiag $ reverse $ board]
                Connectk k board turns = s
        doPseudoRandomMove gen s = pick gen $ legalChildren s       

k=3
kInARow :: Board -> Maybe TwoPlayers
kInARow x = firstMatch $ map (\a->row a Nothing) x
          
firstMatch :: [Maybe TwoPlayers] -> Maybe TwoPlayers
firstMatch [] = Nothing
firstMatch (Nothing:xs) = firstMatch xs
firstMatch ((Just x):_) = Just x

row :: [Maybe TwoPlayers] -> Maybe (TwoPlayers,Int) -> Maybe TwoPlayers
row _ (Just (x, 0)) = Just x
row [] _ = Nothing
row (Nothing:xs) _ = row xs Nothing
row ((Just x):xs) Nothing = row xs (Just (x,k-1))
row ((Just x):xs) (Just (y,n)) | x==y = row xs (Just (x,n-1))
                                               | otherwise = row xs (Just (x, k-1))

kDiag a = map (\(a,b) -> a b) $ zip funs a
funs = id : (map (tail.) funs)
        

--The following is all TicTacToe stuff
data TicTacToe = TicTacToe (Set.Set Int) (Set.Set Int) (Set.Set Int) deriving (Show,Eq)
data GameState = Win TwoPlayers | Stale | InProgress deriving (Show,Eq)
data TwoPlayers = A | B deriving (Read, Show,Eq)
playerList = A:B:playerList :: [TwoPlayers]

instance Game TicTacToe where
	legalChildren (TicTacToe x y z) = if ((Set.size y) == (Set.size z)) 
		then (map (\a -> TicTacToe (Set.delete a x) (Set.insert a y) z)  (Set.toList x))
		else (map (\a -> TicTacToe (Set.delete a x) y (Set.insert a z))  (Set.toList x))

	
	currentState (TicTacToe x y z) 
 		| f $ Set.toList y = Win A
		| f $ Set.toList z = Win B
		| null $ legalChildren $ TicTacToe x y z = Stale --TODO: aliasing here
		| otherwise = InProgress
                where 
                  f a = if (length a) < 3 then False else 15 `elem` (map sum $ choose 3 a)
	          
        doPseudoRandomMove gen board = pick gen (legalChildren board)

choose :: Int -> [a] -> [[a]]  --Produce all the combinations of n elements from a list
choose 0 _ = [[]]
choose _ [] = undefined        --Undefined where n greater than the length of the list
choose n (x:xs) = (map (x:) $ choose (n-1) xs) ++ next
  where next = if n > (length xs) then [] else choose n xs

prop_choose :: LogarithmiclyIncreasingInt -> LogarithmiclyIncreasingList Int -> Property  --Define a property which should hold of the choose function
prop_choose n xs = n' <= length xs' ==>                                                   --Skip test cases where the length of the list shorter than n
                   (prop_len n' xs') .&&. (prop_memb n' xs')
  where 
   prop_len :: Int -> [a] -> Property
   prop_len n xs = printTestCase "Checking outer list has length `n choose k` where n is length of input list and k is the number of elements we are choosing" $ 
              ((fromIntegral $ length xs) `chooseN` (fromIntegral n)) == fromIntegral (length $ choose n xs)
   
   prop_memb :: Int -> [a] -> Property
   prop_memb n xs = printTestCase "Checking inner lists all have a length that is equal to the number of elements we are choosing" $ 
               foldl (\x-> \y-> x && (n==y)) True (map length (choose n xs)) 
   
   LII n' = n
   LIL xs' = xs

   chooseN :: Integer -> Integer -> Integer
   chooseN n k | n>=k = (fact n) `div` ((fact k)*(fact $ n-k))
               | otherwise = undefined 
 
   fact :: Integer->Integer
   fact 0 = 1
   fact 1 = 1
   fact x | x>1 = x*(fact (x-1))
          | otherwise = undefined

newtype LogarithmiclyIncreasingInt = LII Int
newtype LogarithmiclyIncreasingList a = LIL [a]

instance Arbitrary LogarithmiclyIncreasingInt where
         arbitrary = do i<-QuickCheck.sized (\a -> QuickCheck.choose(0,2 * logi a))
                        return $ LII i

instance Arbitrary a => Arbitrary (LogarithmiclyIncreasingList a) where
         arbitrary = do i<-QuickCheck.sized (\a -> QuickCheck.resize (3 * logi a) $ QuickCheck.listOf arbitrary)
                        return $ LIL i

instance Show LogarithmiclyIncreasingInt where
         show (LII i) = show i

instance Show a => Show (LogarithmiclyIncreasingList a) where
         show (LIL l) = show l





startBoard::TicTacToe
startBoard = TicTacToe (Set.fromList [1..9]) (Set.empty) (Set.empty)


simulateNode :: (Eq a, Game a, RandomGen g) => MCT a -> g -> (MCT a, GameState, Bool, g)
simulateNode t g = (doBackpropagation t r, r, False, g')
                       where 
                          (r, g') = simulate (rGame t) g


pick :: RandomGen g => g-> [a] -> (a, g) 
pick _ [] = undefined
pick gen xs = let (number, gen') = randomR (0,(length xs)-1) gen in
	(xs!!number, gen')

pickSplit :: RandomGen g => g-> [a] -> (a, [a], g) 
pickSplit _ [] = undefined
pickSplit gen xs = (b, as++bs, gen')
        where
          (number, gen') = randomR (0,(length xs)-1) gen
	  (as, (b:bs)) = splitAt number xs
simulate :: (Game a, RandomGen g) => a->g->(GameState,g)
simulate x gen = case (currentState x) of InProgress -> let (x', gen') = doPseudoRandomMove gen x in simulate x' gen'
					  _ -> (currentState x, gen)

prop_simulate :: TicTacToe->StdGen->Bool
prop_simulate x g = True


--- TREE SEARCH STUFF
data MCT g = MCT {rGame::g
		, rPlayed::Int
		, rQ::Int
		, rChildList::[MCT g]}

instance (Show a, Game a) => Show (MCT a) where --big Q for SEAN HOLDEN HERE, how can I replace TicTacToe with 'g'
                                  show = (show . (doc 2)) 
                                   where
                                    doc 0 _ = empty
                                    doc n t | (rPlayed t == 0) = (text $ show $ rGame t) <+> (text $ show $ currentState $ rGame t) <+> (text $ "len:") <+> (text $ show $ length $ rChildList t)
                                            | otherwise = (text $ show $ rGame t) <+> (text $ show $ currentState $ rGame t) <+> (int $ rPlayed $ t) <+> (int $ rQ $ t) <+> (text $ "len:") <+> (text $ show $ length $ rChildList t)
                                                        $$(nest 1 $ vcat $ map (doc $ n-1) $ rChildList t)


gameToLeaf :: Game a => a -> MCT a
gameToLeaf game = MCT game 0 0 []

expand :: (Game a, Eq a) => MCT a -> MCT a
expand t = t{rChildList=(map (expand . gameToLeaf) $ legalChildren $ rGame t)}
--note that this terminates by itself because if a game has no legal children then it will be mapping onto the empty list and expand will never be called.  topMcts :: (Game a, Eq a, Show a, RandomGen g) => MCT a -> g -> (MCT a, g)
--basically the same as the standard mcts functuon except it doesnt delete exxhausted nodes. exhaustred nodes may be the best choice at the top level and we wouldn't be able to detect that otherwise 
topMcts t g| (currentState $ rGame t)/= InProgress = (doBackpropagation t{rChildList=[]} (currentState $ rGame t), g)
           | otherwise = (doBackpropagation (t{rChildList=(t'':ts') ++ (filter (\x->null (rChildList x)) (rChildList t))}) resultState, gen')
        where selectionList = filter (\x->not(null (rChildList x))) $ rChildList t
              (t', ts', g') = doSelection selectionList g
              (t'', resultState, _, gen') = mcts t' g'

mcts :: (Game a, Eq a, Show a, RandomGen g) => MCT a -> g -> (MCT a, GameState, Bool, g)
mcts t g| (currentState $ rGame t)/= InProgress = (doBackpropagation t{rChildList=[]} (currentState $ rGame t), (currentState $ rGame t), True, g)
        | 0 == ( rPlayed t ) = simulateNode t g 
        | deleteFlag = (doBackpropagation (t{rChildList=ts'}) resultState, resultState, null ts', gen')   
        | otherwise = (doBackpropagation (t{rChildList=(t'':ts')}) resultState, resultState, False, gen')
        where selectionList = rChildList t 
              (t', ts', g') = doSelection selectionList g
              (t'', resultState, deleteFlag, gen') = mcts t' g'


prop_mcts :: MCT TicTacToe -> StdGen -> Property --with prop_mcts we assume that there is no  
prop_mcts t g = seq (duplicates [t]) (comparison_logic t t' )
    where
       comparison_logic :: (Game a, Eq a, Show a) => MCT a -> MCT a -> Property
       comparison_logic t t' = not (duplicates $ rChildList t) ==>
                               (prop_childlist t t') .&&. (prop_increments t t') 


       prop_childlist t t' | ((currentState $ rGame t) /= InProgress) =                printTestCase "Since game is InProgress, checking that childlist empty" $
                                                                                        (null $ rChildList t')     --No children 
                             
                           | ((rPlayed t) == 0) =                                      printTestCase "Since game tree has 0 play count, checking that childlists of testcase and output have same childlist (this is only a shallow check)" $
                                                                                        ((length $ rChildList t) == (length $ rChildList t')) &&    --This line and following line ensure
                                                                                        (gameListSubset (rChildList t) (rChildList t'))             --Child lists contain same games (shallow check) 
                           
                           | ((length $ rChildList t') == (length $ rChildList t)) =   case (gameListSubsetWithDiscrepency (rChildList t') (rChildList t)) of
                                                                                        Left a ->  a
                                                                                        Right a -> comparison_logic (Maybe.fromMaybe undefined $ getGameFromList a $ rChildList t) (Maybe.fromMaybe undefined $ getGameFromList a $ rChildList t') -- recursive step, can use undefined because we already know that there is that game there
                           | ((length $ rChildList t') == (length $ rChildList t)-1) = printTestCase "Since a deletion has occured, checking no spurious nodes have come in" $
                                                                                        (gameListSubset (rChildList t') (rChildList t))
                           
                           | otherwise =                                               printTestCase "The child lists have got different lengths"False

       prop_increments t t' = printTestCase "Checking that rPlayed gets incremented at the top level" $ 
                              (rPlayed t') == ((rPlayed t)+1)
 
       gameListSubsetWithDiscrepency :: (Game a, Eq a) => [MCT a] -> [MCT a] -> Either Property (MCT a)
       gameListSubsetWithDiscrepency [] _ = Left $ printTestCase "gameListSubsetWithDiscrepency run but no discrepency found" False
       gameListSubsetWithDiscrepency (b:bs) c = case (getGameFromList b c) of Nothing -> Left $ printTestCase "gameListSubsetWithDiscrepency run but was not a subset" False
                                                                              Just a -> if ((rPlayed a) == (rPlayed b)) 
                                                                                        then gameListSubsetWithDiscrepency bs c 
                                                                                        else (if gameListSubset bs c then Right b else Left $ printTestCase "gameListSubsetWithDiscrepency was run and discrepancy found but the rest was not a subset" False)
       
       duplicates ::(Game a, Eq a) => [MCT a] -> Bool
       duplicates [] = False
       duplicates (b:bs) = case (getGameFromList b bs) of Nothing -> duplicates bs
                                                          Just _ -> True
       
       gameListSubset :: (Game a, Eq a) => [MCT a] -> [MCT a] -> Bool
       gameListSubset [] _ = True -- [] is a subset of anything
       gameListSubset (b:bs) c = case (getGameFromList b c) of Nothing -> False
                                                               Just a -> if ((rPlayed a) == (rPlayed b)) then gameListSubset bs c else False
       getGameFromList :: (Game a, Eq a) => MCT a -> [MCT a] -> Maybe (MCT a)
       getGameFromList a [] = Nothing
       getGameFromList a (b:bs) = if ((rGame a) == (rGame b)) then Just b else (getGameFromList a bs)
       
       (t',_,_,_) = mcts t g

doSelection :: (Game a, RandomGen g) => [MCT a] -> g -> (MCT a, [MCT a], g)
doSelection ts g = let (a, b, c) = doComparisons pairs g in (fst a, map fst b, c)
  where
    doComparisons :: (Game a, RandomGen g, Floating b, Ord b) => [(MCT a, b)] -> g -> ((MCT a, b),[(MCT a,b)], g)
    doComparisons [] g = undefined
    doComparisons [x] g = (x,[], g)
    doComparisons (x:xs) g
      | q' == q  = if a then (x',x:xs',g'') else (x,xs,g'') 
      | q' > q = (x',x:xs',g'')
      | otherwise = (x,xs,g'')
        where (t,q) = x
              (t',q') = x'
              (a,g') = random g
              (x', xs', g'') = doComparisons xs g'
    
    ucbScores :: (Game a, Floating b) => [MCT a] -> Int -> [b]
    ucbScores [] _ = []
    ucbScores (t:ts) n = averageScore t + s : (ucbScores ts n)
      where s = sqrt ((log (fromIntegral n)) / (fromIntegral $ rPlayed t))

    pairs = zip ts $ ucbScores ts $ foldr (\x-> ((+) (rPlayed x))) 0 ts

prop_doSelection :: [MCT TicTacToe] -> StdGen -> Property
prop_doSelection ts g = not (null ts) ==>
                        printTestCase "Checking doSelection produces a list of the correct length" $
                        (length ts') == (length ts) - 1
  where 
    (t', ts', g') = doSelection ts g

doBackpropagation :: Game a => MCT a -> GameState -> MCT a --not recurive
doBackpropagation t s = let t' = t{rPlayed = (rPlayed t)+1} in
			case s of Win A -> t'{rQ = (rQ t) + 1}
				  Win B -> t'{rQ = (rQ t) - 1}
				  Stale -> t'
				  InProgress -> t
				  --_ -> error "Encountered undefined state"


iterativeMcts :: (Game a,Show a, Eq a, RandomGen g) => MCT a -> Int -> g -> MCT a
iterativeMcts t 0 g = t
iterativeMcts t n g | foldl (&&) True (map (null . rChildList) (rChildList t)) = t --quit early since we've searched the whole tree and doing any more would be pointless
                    | otherwise = let (a, g') = topMcts t g in iterativeMcts a (n-1) g'

selectBestMove :: Game a => MCT a -> a
selectBestMove t = rGame (foldl (\a->(\b->if (averageScore a) > (averageScore b) then a else b)) (head $ rChildList t) (rChildList t))

averageScore :: (Game a, Floating b) => MCT a -> b
averageScore t | rPlayed t == 0 = 0
               | otherwise = (fromIntegral $ rQ t) / (fromIntegral $ rPlayed t)

instance Arbitrary TicTacToe where
         arbitrary = do (a,b,c) <- split3 [1,2,3,4,5,6,7,8,9]
                        return $ TicTacToe (Set.fromList a) (Set.fromList b) (Set.fromList c)

split3 :: [a] -> QuickCheck.Gen ([a],[a],[a])
split3 [] = return ([],[],[])
split3 (x:xs) = do (a,b,c) <- split3 xs
                   QuickCheck.oneof [return (x:a,b,c), return (a,x:b,c), return (a,b,x:c)]

instance Arbitrary (MCT TicTacToe) where
         arbitrary = QuickCheck.sized $ \n->mkMCT $ ceiling $ log $ (fromIntegral n)+1 --div 2 is a hack. shortens running time.

mkMCT :: Int -> QuickCheck.Gen (MCT TicTacToe)
mkMCT 0 = do game <- arbitrary
             return $ expand $ gameToLeaf $ game --will automatically produce no children if stale. guaranteed to produce children if not stale so we avoid the nasty doSelection [] bug.

mkMCT d = do game <- arbitrary                   --have to check for staleness
             (QuickCheck.Positive plays)  <- arbitrary --never zero due to use of positive
             q <- arbitrary  
             diff <- QuickCheck.choose (1,1 `max` d)
             d' <- return $ d-diff
             children <- if ((currentState game) == Stale) then (return []) else QuickCheck.listOf1 $ mkMCT d' --checking for staleness. listOf1 because if it's not stale then we don't want to produce any games with no children, they would have been deleted otherwise.
             return $ MCT game plays q children
                
instance Arbitrary StdGen where
         arbitrary = do i<-arbitrary
                        return $ mkStdGen i
