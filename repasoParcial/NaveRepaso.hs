import Map
import  MaxHeap

module Nave (Nave , construir, ingresarT, sectoresAsignados, datosDeSector, tripulantesN, agregarASector, asignarASector)
    where

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

{-
    inv.rep: 
    * el sector asignado a un tripulante, existe en la nave.
    * el map y el maxHeap contiene los mismos elementos.
    * los sectores son unicos.
    * los tripulantes de Sector existen en el (map Tripulante) y viseversa.

-}

construir :: [SectorId] -> Nave 
-- O(s log s)
construir ids =  N (crearSectores ids) emptyM emptyH 

crearSectores :: [SectorId] -> Map k v --
crearSectores []       = emptyM 
crearSectores (id:ids) = assocM id (crearS id) (crearSectores ids)

ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mt ht) = case lookupM n mt of
                            Just x  -> (N ms mt ht)
                            Nothing -> let tripulante = crearT n r in
                                   N ms (assocM n tripulante mt) (insertH tripulante ht)          
    
sectoresAsignados :: Nombre -> Nave -> Set SectorId
-- O(log T)
sectoresAsignados n (N ms mt ht) = case lookupM n mt of
                            Just x  -> sectoresT x 
                            Nothing -> error "error, existe tripulante"


datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
-- O(log S)
datosDeSector id (N ms mt ht) = case lookupM id ms of
                                Just x -> (tripulantesS x , componentesS x)
                                Nothing -> error "existe sector"

tripulantesN :: Nave -> [Tripulante]
-- O(log T)
tripulantesN (N _ _ ht) = darTripulantes ht

darTripulantes :: MaxHeap Tripulante-> [Tripulante]
-- O(log T)
darTripulantes emptyH = []
darTripulantes ht = maxH ht : darTripulantes (deleteMaxH th) -- preguntar costo

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N ms mt ht) = let sect = buscarOCrear sid ms
                                     in N (assocM sid (agregarCs cs sect) ms) mt ht

agregarCs :: [Componente] -> Sector -> Sector
agregarCs [] s = s 
agregarCs (c:cs) s = agregarC c (agregarCs cs s)

buscarOCrear :: SectorId -> Map Sector -> Sector
buscarOCrear sid ms = case lookupM sid ms of
                      Just x -> x
                      Nothing -> crearS sid 

asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n sid (N ms mt ht) = let 