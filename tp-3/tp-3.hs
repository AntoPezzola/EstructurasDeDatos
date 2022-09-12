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
poner color CeldaVacia = CeldaVacia
poner color (Bolita co ce) = Bolita co (Bolita color(poner color ce))