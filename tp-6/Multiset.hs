module Multiset
(Multiset , emptyMS , addMS , ocurrencesMS , unionMS , intersectionMS, multiSetToList)
where 

import Map 

data Multiset a = MS Map a Int 

emptyMS :: MultiSet a
addMS :: Ord a => a -> MultiSet a -> MultiSet a
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
multiSetToList :: MultiSet a -> [(a, Int)]

emptyMS  = MS emptyM 
