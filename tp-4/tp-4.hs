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
                              then Capa (aceitunasDuplicadas i) (duplicarAceitunas  p)
                              else Capa i(duplicarAceitunas p)

aceitunasDuplicadas :: Ingrediente -> Ingrediente
aceitunasDuplicadas (Aceitunas n) = Aceitunas (n*2)

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

caminoAlTesoro :: Mapa -> [Dir]
-- PRECOND : existe un tesoro y es ??nico
caminoAlTesoro (Fin c) = []    
caminoAlTesoro (Bifurcacion c mi md) = if hayTesoro mi
                                      then Izq : caminoAlTesoro mi
                                       else Der : caminoAlTesoro md

caminoDeLaRamaMasLarga :: Mapa -> [Dir]  
caminoDeLaRamaMasLarga (Fin c) =  []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = if length (caminoDeLaRamaMasLarga mi) > length (caminoDeLaRamaMasLarga md)
                                               then Izq : caminoDeLaRamaMasLarga mi
                                               else Der : caminoDeLaRamaMasLarga md 
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = []
tesorosPorNivel (Bifurcacion c mi md) = agregarTesorosA c (unirNiveles (tesorosPorNivel mi) (tesorosPorNivel md))

agregarTesorosA :: Cofre -> [[Objeto]] -> [[Objeto]]
agregarTesorosA (Cofre objs) os = losQueSonTesoros objs : os

losQueSonTesoros :: [Objeto] -> [Objeto]
losQueSonTesoros [] = []
losQueSonTesoros (o:os) = if esTesoro o 
                         then o : losQueSonTesoros os
                         else losQueSonTesoros os  

unirNiveles :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
unirNiveles [] oss = oss 
unirNiveles xss [] = xss 
unirNiveles (xs:xss) (os:oss) = (xs ++ os) : unirNiveles xss oss   

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = []
todosLosCaminos (Bifurcacion c mi md) = agregarDir Izq (todosLosCaminos mi) ++ agregarDir Der (todosLosCaminos md)

agregarDir :: Dir -> [[Dir]] -> [[Dir]]
agregarDir d [] = []
agregarDir d (ds:dss) = (d:ds) : agregarDir d dss


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
        deriving Show
data Sector = S SectorId [Componente] [Tripulante]
        deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show
data Nave = N (Tree Sector)
        deriving Show

--------------

nave0 = N treesector0
treesector0 = NodeT sector0 (NodeT sector1 EmptyT EmptyT) EmptyT
sector0 = S "123" comp0 trip0
comp0 = [LanzaTorpedos, (Motor 50), (Almacen [Comida, Oxigeno, Combustible]), LanzaTorpedos]
trip0= ["a", "b", "c", "d"]

sector1 = S "345" comp1 trip1
comp1 = [(Motor 100), LanzaTorpedos]
trip1 = ["w","e","q"]
comp2 = [LanzaTorpedos, (Motor 150)]
sect= ["345", "123"]


sectores :: Nave -> [SectorId]
sectores (N t) = losSectoresDe t 
  
losSectoresDe :: Tree Sector  -> [SectorId]
losSectoresDe EmptyT =  []
losSectoresDe (NodeT s t1 t2) = idSector s : losSectoresDe t1 ++ losSectoresDe  t2

idSector :: Sector -> SectorId
idSector (S id _ _ )  = id 


poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = propulsion t

propulsion :: Tree Sector -> Int
propulsion EmptyT = 0
propulsion (NodeT s t1 t2) = compontnesSector s + propulsion t1 + propulsion t2

compontnesSector :: Sector -> Int
compontnesSector (S _ comp _ ) = compSectorTotal comp

compSectorTotal :: [Componente] -> Int
compSectorTotal [] = 0
compSectorTotal (c:cs) = propulsionC c + compSectorTotal cs

propulsionC :: Componente -> Int
propulsionC (Motor n) = n 
propulsionC _         = 0

{-
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs s (N t) = naveConSectorAgregado cs s t
naveConSectorAgregado :: [Componente] -> SectorId -> Tree Sector -> Nave
naveConSectorAgregado cs s EmptyT = como devuelvo una nave?
-} 

barriles :: Nave -> [Barril]
-- Prop: devuelve todos los barriles de la nave
barriles (N t) = barrilesDeLaNave t

barrilesDeLaNave :: Tree Sector -> [Barril] 
barrilesDeLaNave EmptyT = []
barrilesDeLaNave (NodeT s t1 t2) =  losBarrilesDelSector s ++ barrilesDeLaNave t1  ++ barrilesDeLaNave t2 

losBarrilesDelSector :: Sector -> [Barril] 
losBarrilesDelSector (S _ comp _ ) = losBarriles comp

losBarriles :: [Componente] -> [Barril]
losBarriles [] = []
losBarriles (c:cs) = barrilesEnComponente c ++ losBarriles cs

barrilesEnComponente :: Componente -> [Barril]
barrilesEnComponente (Almacen bs) = bs

-------------
{-
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs s (N t) = naveConSectorAgregado cs s t

naveConSectorAgregado :: [Componente] -> SectorId -> Tree Sector -> Nave
naveConSectorAgregado cs s EmptyT = como devuelvo una nave?

-} 
-------------

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N tr) = losSectoresConTripulanteEnNave t tr

losSectoresConTripulanteEnNave :: Tripulante -> Tree Sector -> [SectorId]
losSectoresConTripulanteEnNave t          EmptyT = []
losSectoresConTripulanteEnNave t (NodeT s s1 s2) = if sectorConTripulante s t 
                              then idSector s : losSectoresConTripulanteEnNave t s1 ++ losSectoresConTripulanteEnNave t s2
                              else losSectoresConTripulanteEnNave t s1 ++ losSectoresConTripulanteEnNave t s2

sectorConTripulante :: Sector -> Tripulante -> Bool
sectorConTripulante (S _ _ tripu ) t =  estaElTripulante t tripu

estaElTripulante :: Tripulante -> [Tripulante] -> Bool
estaElTripulante tri (t:ts) = esElMismoTripulante tri t || estaElTripulante tri ts

esElMismoTripulante :: Tripulante -> Tripulante -> Bool
esElMismoTripulante t t1 = t == t1

tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripulantesSinRepetir(losTripulantesDeLaNave t)

losTripulantesDeLaNave :: Tree Sector -> [Tripulante]
losTripulantesDeLaNave EmptyT = []
losTripulantesDeLaNave (NodeT s t1 t2) = losTripulantes s ++  losTripulantesDeLaNave t1  ++ losTripulantesDeLaNave t2 

losTripulantes :: Sector -> [Tripulante]
losTripulantes  (S _ _ tripu ) = tripu

tripulantesSinRepetir :: [Tripulante] -> [Tripulante]
tripulantesSinRepetir []     = []
tripulantesSinRepetir (t:ts) = if tripulanteRepetidos t ts 
                               then tripulantesSinRepetir ts
                               else t : tripulantesSinRepetir ts

tripulanteRepetidos :: Tripulante -> [Tripulante] -> Bool
tripulanteRepetidos y (x:xs) = esElMismoTripulante y x || tripulanteRepetidos y xs

-----------------

type Presa      = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre     = String -- nombre de lobo
data Lobo   = Cazador Nombre [Presa] Lobo Lobo Lobo  | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
                deriving Show
data Manada = M Lobo
                deriving Show

lobo0 = Cria "cria1"
lobo1 = Cria "cria2"
lobo2 = Cria "cria5"
lobo3 = Cria "cria3"
lobo9 = Cria "cria4"

lobo4 = Cazador "Cazador1" ["p1", "p2","p4", "p3"] lobo7 lobo8 lobo10
lobo5 = Cazador "Cazador2" ["p2","p4","p0", "p2","p4", "p3"] lobo7 lobo0 lobo10
lobo6 = Cazador "Cazador3" ["p1","p9", "p3", "p2","p4", "p3"] lobo8 lobo5 lobo0

lobo7 = Explorador "Explorador1" ["Quilmes", "Bernal", "Wilde"] lobo0 lobo10
lobo8 = Explorador "Explorador2" ["Avellaneda", "Sarandi", "Vill D??minico"] lobo7 lobo0
lobo10 =Explorador "Explorador3" ["Martinez", "Palermo", "Puerto Madero"] lobo0 lobo3

manada0 = M lobo4

buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M lobo) = cantidadDeAlimentoL lobo

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cazador _ presas l1 l2 l3) = alimentoEn presas
                                                + cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
                                                + cantidadDeAlimentoL l3
cantidadDeAlimentoL (Explorador _ _ l1 l2)      = cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cria _)                    = 0

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M lobo) = cantidadDeCriasL lobo 

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Cazador _ _ l1 l2 l3)      = cantidadDeCriasL l1 + cantidadDeCriasL l2 + cantidadDeCriasL l3 
cantidadDeCriasL (Explorador _ _ l1 l2)      = cantidadDeCriasL l1 + cantidadDeCriasL l2
cantidadDeCriasL (Cria _)                    =  1 

alimentoEn :: [Presa] -> Int
alimentoEn ps = length ps 

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaL lobo

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador nom presas l1 l2 l3) = elegirEntre (nom, alimentoEn presas)
                                                    (elegirEntre (elAlfaL l1)
                                                                 (elegirEntre (elAlfaL l2)
                                                                              (elAlfaL l3)))
elAlfaL (Explorador nom _ l1 l2)      = elegirEntre (elAlfaL l1)
                                                    (elegirEntre (elAlfaL l2)
                                                                 (nom, 0))
elAlfaL (Cria nom)                    = (nom, 0)


elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (nom1, c1) (nom2, c2) = if (c1>=c2) then (nom1, c1)
                                                else (nom2, c2)


elegir :: [ (Nombre, Int) ] -> (Nombre, Int)
-- PRECOND: la lista no es vac??a
elegir (nc : [])  = nc
elegir (nc : ncs) = elegirEntre nc (elegir ncs)


losExploradoresQuePasaronPorTerritorio :: Territorio -> Lobo -> [Nombre]
losExploradoresQuePasaronPorTerritorio t (Cria _ ) = []
losExploradoresQuePasaronPorTerritorio t (Cazador _ _ l1 l2 l3) =  losExploradoresQuePasaronPorTerritorio t l1 ++
                                                                   losExploradoresQuePasaronPorTerritorio t l2 ++ 
                                                                   losExploradoresQuePasaronPorTerritorio t l3
losExploradoresQuePasaronPorTerritorio t (Explorador n te l1 l2 ) = agregarNombreSiEstaElTerritorio t te n (losExploradoresQuePasaronPorTerritorio t l1 ++
                                                                   losExploradoresQuePasaronPorTerritorio t l2)  

agregarNombreSiEstaElTerritorio :: Territorio -> [Territorio] -> Nombre -> [Nombre] -> [Nombre]
agregarNombreSiEstaElTerritorio t ts n ns = if estaElTerritorio t ts
                                            then n : ns 
                                            else ns 

estaElTerritorio :: Territorio -> [Territorio] -> Bool
estaElTerritorio t [] = False 
estaElTerritorio t (te:tes) =  t == te || estaElTerritorio t tes   

