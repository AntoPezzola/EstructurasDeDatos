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

apariciones :: Eq a => a -> [a] -> Int
apariciones  a []  = 0 
apariciones a (x:xs)  =  unoSi( a == x ) + apariciones a xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA a [] = []
losMenoresA a (x:xs) = if x < a
                       then  x : losMenoresA a xs
                       else losMenoresA a xs 


lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA a [] = []
lasDeLongitudMayorA a (xs:xss) = if longitud xs > a 
                                then xs : lasDeLongitudMayorA a xss
                                else lasDeLongitudMayorA a xss

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a = [a]
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar xs [] = xs
agregar (x:xs) ys = x : agregar xs ys 

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal xs x 