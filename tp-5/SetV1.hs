module SetV1
  (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
  where 

data Set a = S [a] 

emptyS :: Set a
addS :: Eq a => a -> Set a -> Set a
belongs :: Eq a => a -> Set a -> Bool
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]


emptyS         = S []
addS    x (S xs)      = S (x:xs)   --O(1)
belongs x (S xs)      = pertenece x xs  -- O(1)
-- sizeS     (S xs)      = cantidadDeDistintos xs 
-- removeS x (S xs)      = S (sinElElemento x xs) 
unionS  (S xs) (S ys) = S(xs ++ ys)  -- O(n)
setToList (S xs)      = losElementosSinRepetir xs --O(n^2)   
lenS        (S xs)    = lenght (losElementosSinRepetir xs) -- O(n^2)


pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = a == x || pertenece a xs

cantidadDeDistintos :: Eq a => [a] -> Int 
cantidadDeDistintos [] = 0
cantidadDeDistintos (x:xs) = if  pertenece x xs 
                            then cantidadDeDistintos xs 
                            else 1 + cantidadDeDistintos xs

sinElElemento :: Eq a => a -> [a] -> [a]
sinElElemento _ [] = []
sinElElemento x (y:ys) = if x == y
                        then sinElElemento x ys
                        else y : sinElElemento x ys 

losElementosSinRepetir :: Eq a => [a] -> [a]
losElementosSinRepetir [] = []
losElementosSinRepetir (x:xs) = if  pertenece x xs 
                                then losElementosSinRepetir xs 
                                else x : losElementosSinRepetir xs


