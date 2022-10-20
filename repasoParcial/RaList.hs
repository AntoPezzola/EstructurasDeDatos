import Map 
import Heap

module RAList (RAList , emptyRAL, isEmptyRAL, lengthRAL, get, minRAL, add, elems, remove, set, addAt)
    where 

data RAList a = MkR Int (Map Int a) (Heap a)

{-
  INV.REP: 
    -   Todos los elementos que estan en el map, estan el heap y viceversa.
    -   El numero que sea Int, debe ser el mismo que la cantidad de elementos del map.
    -   Heap no puede tener elementos repetidos .

-}



emptyRAL :: RAList a
-- o(1)
emptyRAL = MkR 0 emptyM emptyH

isEmptyRAL :: RAList a -> Bool
-- o(1)
isEmptyRAL (MkR i _ _) = i > 0 

lengthRAL :: RAList a -> Int
-- o(1)
lengthRAL (MkR i _ _) = i - 1

get :: Int -> RAList a -> a
get x (MkR i mi hi) = case lookupM x mi of
                      Just y -> y
                      Nothing -> error "el elemento existe"

minRAL :: Ord a => RAList a -> a
minRAL (MkR i mi hi) = findMin hi

add :: Ord a => a -> RAList a -> RAList a
-- O(log K)
add x (MkR i mi hi) = MkR (i+1) (assocM i x mi) (insertH x hi)

elems :: Ord a => RAList a -> [a]
elems (MkR i mi hi) = listToMap (i-1) mi

listToMap :: Int -> Map k v -> [a]
-- preguntar
listToMap i emptyM = []
listToMap 0 _ = []
listToMap i m = reverse ((lookupM i m) : (listToMap (i-1) m))
--                        (10, k)      :  ()()()
listToMap i m = (listToMap (i-1) m) : (lookupM i m)
--               (8, g)(9, h)       : (10, k)

remove :: Ord a => RAList a -> RAList a
-- preguntar sobre (i-1)
remove (MkR i mi hi) = let e = lookupM (i-1) mi in
                        MkR (i-1) (deleteM (i-1) mi) (rearmarHSin e hi)


rearmarHSin :: Ord a => a -> Heap a -> Heap a
rearmarHSin e hi = if e == findMin hi
                    then deleteMin hi
                    else insertH (findMin ih) (rearmarHSin e (deleteMin hi))


set :: Ord a => Int -> a -> RAList a -> RAList a
set x e (MkR i mi hi) = let ev = fromJust (lookupM mi x) in
                        MkR i (assocM i e mi) (reemplazar ev e hi)


reemplazar :: Ord a => a -> a -> Heap a -> Heap a
reemplazar ev en hi = if ev == findMin hi
                      then insertH en (deleteMin hi)
                      else insertH (findMin hi) (reemplazar ev en (deleteMin hi))


addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt k e (MkR i mi hi) = MkR (i+1) (assocM k e (desplazar i k mi)) (insertH e hi)


desplazar :: Ord => Int -> Int ->  Map Int a -> Map Int a 
desplazar i t mp =  if i == t 
                    then mp 
                    else assocM (i + 1) (formJust(lookupM mp i)) (desplazar (i + 1) t mp )


