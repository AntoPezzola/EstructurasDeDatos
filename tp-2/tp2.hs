sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
longitud [] = 0 
longitud (n:ns) = 1 + longitud ns

sucesores :: [Int] -> [Int]
sucesores [] = [] 
sucesores (n:ns) = n+1 : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs 

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (a:as) = a ++ aplanar as

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = a == x || pertenece a xs