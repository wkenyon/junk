import Control.Monad.State

type Var = Char

type E = State [(Var,Expr)]

insertHeap :: Var -> Expr -> E -> E
insertHeap x e (State f) = (x,e):xs

lookupHeap :: Var -> E -> Expr
lookupHeap x [] = undefined
lookupHeap x ((y,e):ys) | y == x    = e
                        | otherwise = lookupHeap x ys

deleteHeap :: Var -> [(Var,Expr)] -> [(Var,Expr)]
deleteHeap x [] = undefined
deleteHeap x ((y,e):ys) | y == x    = ys
                        | otherwise = deleteHeap x ys

subst :: Var -> Var -> Expr -> Expr
subst x y (EInt n) = EInt n
subst x y (ELam z e) = ELam z (subst x y e)
subst x y (ELet (z,e1) e2) = ELet (z,(subst x y e1)) (subst x y e2)

data Expr = EInt Int
          | ELam Var Expr
          | ELet (Var,Expr) Expr
          | EApp Expr Var --only permit applications to variables
          | EVar Var 


eval :: Expr -> E Expr
eval (EInt x) = return (EInt x)
eval (ELam x e) = return (ELam x e)
eval (ELet (x,e1) e2) = do
                          insertHeap x e1
                          eval e2

eval (EApp f x) = do
                    ELam y e <- eval f
                    eval (subst y x e)

eval (EVar x) = do 
                  r <- eval lookupHeap x
                  case r of
                    EInt i -> return $ EInt i
                    ELam y e -> return ELam y e
                    e -> do
                      deleteHeap x
                      v <- eval e
                      insertHeap x v
                      eval (EVar x)
                    

