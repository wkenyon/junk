import Data.List
import Data.Maybe
import Control.Monad.State

data Var = Var Int
  deriving (Eq, Show)
type Heap = [(Var,Expr)]
type Context = (OperationalStack,Heap)
type Label = [String]

type CostCenterStack = [Label]
type OperationalStack = [Continuation]

type ConsPattern = (String, ([Var],Expr))
type Cons = (String, [Var])

data Continuation = ApplyTo Var
                  | Update Var 
                  | Case [ConsPattern]
                      deriving Show

data Expr = EInt Int
          | ELam Var Expr
          | ELet (Var,Expr) Expr
          | EApp Expr Var
          | EVar Var
          | ECons Cons 
          | ECase Expr [ConsPattern]
              deriving (Eq, Show)

getHeap :: State Context Heap
getHeap = state $ \(ostack,heap) -> (heap,(ostack,heap))
putHeap :: Heap -> State Context ()
putHeap heap = state $ \(ostack,_) -> ((),(ostack,heap))

getOStack :: State Context OperationalStack
getOStack = state $ \(ostack,heap) -> (ostack,(ostack,heap))
putOStack :: OperationalStack -> State Context ()
putOStack ostack = state $ \(_,heap) -> ((),(ostack,heap))



insertHeap :: Var -> Expr -> State Context ()
insertHeap x e = do xs <- getHeap
                    putHeap ((x,e):xs)

deleteHeap :: Var -> State Context ()
deleteHeap x = do xs <- getHeap 
                  putHeap [ y | y <- xs, ((fst y) /= x) ]

lookupHeap :: Var -> State Context Expr
lookupHeap x = do xs <- getHeap 
                  return $ fromJust $ lookup x xs

pushOStack :: Continuation -> State Context ()
pushOStack x = do xs <- getOStack
                  putOStack (x:xs)

popOStack :: State Context Continuation
popOStack = do xs <- getOStack
               putOStack $ tail xs
               return $ head xs

emptyOStack :: State Context Bool
emptyOStack = do xs <- getOStack
                 return $ null xs

subst :: Var -> Var -> Expr -> Expr
subst x y (EInt n) = EInt n
subst x y (ELam z e) = ELam z (subst x y e)
subst x y (ELet (z,e1) (e2)) = ELet (z,(subst x y e1)) (subst x y e2)
subst x y (EApp e z) | x==z = EApp (subst x y e) y
                     | otherwise = EApp (subst x y e) z

subst x y (EVar z) | x==z = EVar y
                   | otherwise = EVar z

subst x y (ECons (s, xs)) = (ECons (s, map f xs))
  where f z | z==y        = x
            | otherwise   = z

subst x y (ECase e xs) = ECase (subst x y e) $ map f xs
  where f (s,(ss,e2)) = (s,(ss,subst x y e2))
  

eval :: Expr -> State Context Expr
--To evaluate base values we must apply a continuation (from the operational stack):
eval (EInt x) = continue $ EInt x
eval (ELam x e) = continue $ ELam x e
eval (ECons (s, xs)) = continue $ ECons (s,xs)

--let expressions add to the heap
eval (ELet (x,e1) e2) = do insertHeap x e1
                           eval e2

eval (EApp e x) = do pushOStack $ ApplyTo x
                     eval e

eval (ECase e xs) = do pushOStack $ Case xs
                       eval e

eval (EVar x) = do 
  r <- lookupHeap x
  case r of
    ELam y e -> continue $ ELam y e
    e -> do deleteHeap x
            pushOStack $ Update x
            eval e

continue :: Expr -> State Context Expr
continue v = do x<-emptyOStack
                if x then (return v) else do
                  y<-popOStack
                  f y v
  where
    f (ApplyTo x) (ELam y e) = eval (subst y x e)
    f (Update x) v = do insertHeap x v
                        continue v
    f (Case xs) (ECons (name, ys)) = do 
      let (ys',e) = fromJust $ lookup name xs
      eval $ foldl (\e1-> \(y',y) -> subst y' y e1) e (zip ys' ys)
      
    f ctinue v = error(show v)


-- ELet (Var 1, ELam (Var 2) $ EVar $ Var 2) $ EApp (EVar $ Var 1) $ EInt 2
-- ELet (Var 1, ELam (Var 2) $ 

-- ECons ("Nil",[])
ELet (Var 100, EInt 1) $ 
ELet (Var 101, EInt 2) $
ELet (Var 102, EInt 3) $
ELet (Var 110, EInt 4) $
ELet (Var 111, EInt 5) $
ELet (Var 203, ECons ("Nil", []) $
ELet (Var 202, ECons ("Cons",[Var 102,Var 203]) $
ELet (Var 201, ECons ("Cons",[Var 101,Var 202]) $
ELet (Var 200, ECons ("Cons",[Var 100,Var 201]) $
ELet (Var 212, ECons ("Nil", []) $
ELet (Var 211, ECons ("Cons",[Var 111,Var 212]) $
ELet (Var 210, ECons ("Cons",[Var 110,Var 211]) $


ECons ("Cons",[Var 200, Var 100])



