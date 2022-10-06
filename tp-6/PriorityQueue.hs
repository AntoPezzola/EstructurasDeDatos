module PriorityQueue
  (PriorityQueue, emptyPQ , isEmptyPQ, insertPQ , findMinPQ , deleteMinPQ)
where 

data PriorityQueue a = PQ [a]
  deriving Show 

emptyPQ :: PriorityQueue a
isEmptyPQ :: PriorityQueue a -> Bool
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
findMinPQ :: Ord a => PriorityQueue a -> a
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a


emptyPQ              = PQ []
isEmptyPQ    (PQ xs) = null xs
insertPQ   x (PQ xs) = PQ (x:xs)
findMinPQ  (PQ xs)   = minimum xs --  PARCIAL si xs es vacía
deleteMinPQ (PQ xs)  = PQ (borrarMin xs) -- PARCIAL si la lista es vacía


borrarMin :: Ord a => [a] -> [a] 
-- PRECOND: la lista no es vacía
borrarMin xs = borrar (minimum xs) xs 
                      -- minimo elemento 

borrar :: Eq a => a -> [a] -> [a]
borrar _ [] = []
borrar x (y:ys) = if x == y 
                  then borrar x ys
                  else y : borrar x ys 