{-
head' :: [a] -> a
head' (x:xs) = x -- costo constante

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 -- costo constante 

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) -- costo lineal

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs -- costo lineal

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs -- costo cuadraticos

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs 

-} 

import SetV1 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
            deriving Show

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _  = []
losQuePertenecen (x:xs) set = if belongs x set
                            then x : losQuePertenecen xs set
                            else losQuePertenecen xs set 

sinRepetidos :: Eq a => [a] -> [a]
-- PROP: Quita todos los elementos repetidos de la lista dada utilizando 
-- un conjunto como estructura auxiliar.
sinRepetidos ts = setToList (listaSet ts)

listaSet :: [a] -> Set a 
listaSet [] =  emptyS
listaSet (x:xs) = addS x (listaSet xs) 

unirTodos :: Eq a => Tree (Set a) -> Set a
--PROP:Dado un arbol de conjuntos devuelve un conjunto con la union 
-- de todos los conjuntos del arbol.
unirTodos EmptyT      = emptyS 
unirTodos (NodeT s set set1) =  unionS s (unionS (unirTodos set) (unirTodos set1))  

