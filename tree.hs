data Tree a = Empty
	    | Node a (Tree a) (Tree a)
		deriving (Show, Read, Eq)

singleton :: a->Tree a
singleton x = Node x Empty Empty

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node y t1 t2)
	| x==y = Node y t1 t2
	| x<y = Node y (treeInsert x t1) t2
	| x>y = Node y t1 (treeInsert x t2)

instance Functor Tree where
	fmap _ Empty = Empty
	fmap f (Node x t1 t2) = Node (f x) (fmap f t1) (fmap f t2)

