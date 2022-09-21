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
esAceituna Aceitunas Int = True 