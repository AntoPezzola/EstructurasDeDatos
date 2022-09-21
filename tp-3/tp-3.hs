data Color = Azul | Rojo
        deriving Show
data Celda = Bolita Color Celda | CeldaVacia
        deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita co ce) = unoSi(esDeColor co) + nroBolitas co ce

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

esDeColor :: Color -> Bool
esDeColor c = True
esDeColor _ = False

celda2 = CeldaVacia
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda3 = Bolita Rojo (Bolita Azul CeldaVacia)
celda4 = Bolita Azul (Bolita Rojo  CeldaVacia)


poner :: Color -> Celda -> Celda
poner color celda =  Bolita color celda

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar color (Bolita co ce) = if esDeColor color
                             then Bolita(sacar co ce)
                             else Bolita co (sacar color ce) 

ponerN :: Int -> Color -> Celda -> Celda
ponerN  0 color celda = celda
ponnerN n color celda = Bolita color(ponerN (n-1) color celda)  

--- Camino hacia el tesoro ---

data Objeto = Cacharro | Tesoro
        deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
        deriving Show

camino0 :: Camino
camino0 = Fin

camino1 :: Camino 
camino1 = Cofre [Tesoro] (Nada (Cofre [Cacharro] (Cofre [Tesoro,Tesoro] Fin)))

camino2 :: Camino
camino2 = (Nada (Cofre [Tesoro,Cacharro] Fin))


hayTesoro :: Camino -> Bool
hayTesoro  fin            = False
hayTesoro  (Cofre objs c) = tieneTesoro objs|| hayTesoro c -- si tiene tesoro el obj, sino sigue la recu
hayTesoro  (Nada c)       = False

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (x:xs) = esTesoro x || tieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
-- PRECOND: tiene que haber al menos un tesoro
pasosHastaTesoro   fin           =  "error debe haber un tesoro"
pasosHastaTesoro  (Cofre objs c) =  if hayTesoro c
                                    then 0
                                    else 1 + pasosHastaTesoro c
pasosHastaTesoro  (Nada c)       =  1 + pasosHastaTesoro c  -- no hay nada, sigo la recurcion    

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n fin = False
hayTesoroEn n (Nada c) = hayTesoroEn (n-1) c 
hayTesoroEn (Cofre objs c) = if tieneTesoro objs 
                             then n == 0
                             else hayTesoroEn (n-1) c  

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n fin          =  n <= 0
alMenosNTesoros n (Nada c)     = alMenosNTesoros (n-1) c
alMenosNTesoros (Cofre objs c) = if tieneTesoro objs 
                                 then alMenosNTesoros (n-1) c
                                 else alMenosNTesoros n      -- si no tiene tesoro no va a restar, y si tiene le saco hasta que llegue
                                                                        -- en caso q cumpla 
--- Tipos arbóreos ---

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
            deriving Show


arbol1 = NodeT 1 (NodeT 1 ) (NodeT 1) 

sumarT :: Tree Int -> Int
sumarT  EmptyT = 0
sumarT  (NodeT n n1 n2) = n + sumarT n1 + sumarT n2 

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT a a1 a2) = 1 + sizeT a1 + sizeT a2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT EmptyT = 0
aparicionesT x (NodeT a a1 a2) = if (x == a)
                                 then 1 + aparicionesT x a1 + aparicionesT x a2
                                 else aparicionesT x a1 + aparicionesT x a2

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT e t1 t2) = if (esUnaHoja t1) && ( esUnaHoja t2 )
                         then e : leaves t1 ++ leaves t2
                         else leaves t1 ++ leaves t2

esUnaHoja :: Tree a -> Bool
esUnaHoja EmptyT = True
esUnaHoja   _    = False

heighT :: Tree a -> Int
heighT EmptyT = 0
heighT (NodeT e t1 t2) = 1 + max (heighT t1) (heighT t2)

mapDobleT :: Tree Integer -> Tree Integer
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT a a1 a2) = (NodeT (a*2) (mapDobleT a1) (mapDobleT a2))

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (NodeT a a1 a2) = x == a || perteneceT x a1 || perteneceT x a2

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT 
mirrorT (NodeT a a1 a2) = (NodeT a (mirrorT a2) (mirrorT a1)) 

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT a a1 a2) = (toList a1) ++ [a] ++ (toList a2) 

levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN 0 (NodeT a a1 a2) = [a]
levelN n (NodeT a a1 a2) =  (levelN (n-1) a1) ++ (levelN (n-1) a2)

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT a a1 a2) = [a] : unirNiveles (listPerLevel a1) (listPerLevel a2)

unirNiveles :: [[a]] -> [[a]] -> [[a]]
unirNiveles [] xss = xss
unirNiveles yss [] = yss
unirNiveles (xs:xss) (ys:yss) = (xs ++ ys) : unirNiveles xss yss 
                        -- los elementos son listas, los uno 

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT a a1 a2) = a : elegirLaRamaMasLarga(ramaMasLarga a1) (ramaMasLarga a2)

elegirLaRamaMasLarga :: [a] -> [a] -> [a]
elegirLaRamaMasLarga _ x = x
elegirLaRamaMasLarga y _ = y
elegirLaRamaMasLarga (x:xs) (y:ys) = if length xs > length ys
                                    then x:xs
                                    else y:ys


 -- data maybe = Just un elemento m da el tipo

 data ExpA = Valor Int  | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
                deriving Show

eval :: ExpA -> Int
eval Valor n       = n
eval (Sum e2 e1 )  = (eval e2) + (eval e1)
eval (Prod e1 e2 ) = (eval e1) * (eval e2)
eval (Neg e1)      = - (eval e1)