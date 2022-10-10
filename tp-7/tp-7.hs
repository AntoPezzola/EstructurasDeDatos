data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

arbol1 = NodeT 10 (NodeT 7 EmptyT EmptyT) (NodeT 20 EmptyT EmptyT) 

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _  EmptyT = False
belongsBST y (NodeT x ti td ) = if (x==y) then True
                                else if (x > y)
                                     then belongsBST y ti 
                                else belongsBST y td 

