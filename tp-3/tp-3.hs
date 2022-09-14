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
                             then ce
                             else sacar color ce -- duda

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
leaves (NodeT x x1 x2) = x : (leaves x1 ++ leaves x2)

