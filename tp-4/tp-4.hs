data Pizza = Prepizza | Capa Ingrediente Pizza
        deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
            deriving Show

pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) 
              (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Queso 
              (Capa Jamon (Capa Jamon 
                                      (Capa Queso (Capa Jamon Prepizza))))

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p 


armarPizza :: [Ingrediente] -> Pizza
armarPizza []  =  Prepizza
armarPizza (i:is) = Capa i (armarPizza is)  


sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza       = Prepizza
sacarJamon (Capa i pi) =    if esJamon i
                            then sacarJamon pi  
                            else Capa i (sacarJamon pi)

esJamon :: Ingrediente -> Bool
esJamon  Jamon = True
esJamon  _    = False

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True 
tieneSoloSalsaYQueso (Capa ing pi) = esQuesoOSalsa ing && tieneSoloSalsaYQueso pi

esQuesoOSalsa :: Ingrediente -> Bool
esQuesoOSalsa Salsa = True
esQuesoOSalsa Queso = True
esQuesoOSalsa _     = False

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas   Prepizza = Prepizza
duplicarAceitunas (Capa i p) = if esAceituna i
                              then Capa i(Capa i (duplicarAceitunas p))
                              else Capa i(duplicarAceitunas p)

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas n) = True 
esAceituna   _           = False

longitud :: [a] -> Int
longitud [] = 0 
longitud (n:ns) = 1 + longitud ns


cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza  []     = []
cantCapasPorPizza  (p:ps) =  (cantidadDeCapas p,p) : cantCapasPorPizza ps
                                               -- devuelvo el int y la pizza
                                               
 data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

cofre1 = Cofre [Chatarra, Chatarra, Tesoro] 

cofre2 = Cofre [Chatarra, Chatarra, Tesoro]

cofre3 = Cofre [Tesoro,Chatarra, Chatarra, Chatarra, Tesoro]

cofre5 = Cofre []

mapa1 = Fin cofre1
mapa2 = Fin cofre2

mapa3 = Bifurcacion cofre3 mapa1 mapa2

mapa4 = Bifurcacion cofre5 mapa1 mapa3


dir = [Der,Der,Izq]

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = tieneTesoro c 
hayTesoro (Bifurcacion c mi md ) = tieneTesoro c || hayTesoro mi || hayTesoro md    

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre os ) = elObjetoTieneTesoro os 

elObjetoTieneTesoro :: [Objeto] -> Bool
elObjetoTieneTesoro [] = False 
elObjetoTieneTesoro (o:os) = esTesoro o || elObjetoTieneTesoro os

esTesoro :: Objeto -> Bool
esTesoro  Tesoro = True
esTesoro   _     = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Bifurcacion c mi md ) = False
hayTesoroEn _ (Fin c) = tieneTesoro c 
hayTesoroEn (d:ds) (Bifurcacion c mi md) = if (esIzquierda d) 
                                        then  hayTesoroEn ds mi
                                        else hayTesoroEn ds md 

esIzquierda :: Dir -> Bool
esIzquierda  Izq = True
esIzquierda   _ = False 
