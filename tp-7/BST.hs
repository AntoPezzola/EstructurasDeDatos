module BST 
 (BST, belongsBST , insertBST , deleteBST , splitMinBST , splitMaxBST , esBST , elMaximoMenorA , elMinimoMayorA 
            , balanceado)

 where 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

arbol1 = NodeT 10 (NodeT 7 EmptyT EmptyT) (NodeT 20 EmptyT EmptyT) 

belongsBST :: Ord a => a -> Tree a -> Bool
insertBST :: Ord a => a -> Tree a -> Tree a
deleteBST :: Ord a => a -> Tree a -> Tree a
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
esBST :: Tree a -> Bool
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
balanceado :: Tree a -> Bool

belongsBST x (Tree t) = buscarBST x t 
-- Prop: dado un BST dice si el elemento pertenece o no al árbol
insertBST  x (Tree t) = Tree (insertarBST x t) 
-- Prop: dado un BST inserta un elemento en el árbol
deleteBST x (Tree t) = Tree (borrarBST x t)
-- Prop: dado un BST borra un elemento en el árbol.


buscarBST :: Ord => a -> Tree a -> Bool
buscarBST _  EmptyT = False
buscarBST y (NodeT x ti td ) = if (x==y) then True
                                else if (x > y)
                                     then buscarBST y ti 
                                else buscarBST y td 

insertarBST :: Ord => a -> Tree a -> Tree a
insertarBST y EmptyT = NodeT  y EmptyT EmptyT  
insertarBST y (NodeT x ti td) = if (x==y) then NodeT y ti td
                                 else if (x > y) Node x (insertarBST y ti) td  
                                 else Node x ti (insertarBST td)


deleteBST :: Ord => a -> Tree a -> Tree a  
deleteBST y EmptyT = 
deleteBST y (NodeT x ti td) = 

                    