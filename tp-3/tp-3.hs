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
                             else sacar co ce -- duda

ponerN :: Int -> Color -> Celda -> Celda
ponerN  0 color celda = celda
ponnerN n color celda = Bolita color(ponerN (n-1) color celda)  

--- Camino hacia el tesoro ---

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino


hayTesoro :: Camino -> Bool
hayTesoro  fin = ...
hayTesoro nada camino = ...
hayTesoro 