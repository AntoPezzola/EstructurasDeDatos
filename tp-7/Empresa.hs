import Map 
import Set
import Empleado

module Empresa ( 
     Empresa , consEmpleado , cuil , incorporarSector , sectores
)

where 

    data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

{-
  INV.REP 
   - Los empleados que son valores asociados a algún sector, aparecen como valores en el map de empleados
   - Los sectores que tiene asignado el emplado en el segundo map, existen como sectores en el primer map
   - En el primer map, todos los empleados que tengan un sector asignado, estan asociados al mismo
   - El cuil que tenga el emplado, debe estar asociado como clave en el segundo map

-}

consEmpresa :: Empresa
--Prop:construye una empresa vacía.
--Costo: O(1)
buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Prop:devuelve el empleado con dicho CUIL.
--Costo: O(log E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(logS + E)
todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E)
todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S)
agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(logS)
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Prop:agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado.
--Costo:  
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: 
borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: 

consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM 

buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ ce) = case lookupM ce c of 
                               Just e -> e 
                               Nothing -> error "no existe el empleado con dicho cuil"

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- costo de lookupM es de costo log S, ya que en el peor caso recorre todos los sectores de la empresa, y set2list
-- es E , ya que en el peor caso debe recorrer todo el set emplados, por ende el costo temrina siendo O(logS + E)
empleadosDelSector sid (ConsE ses ce) = case lookupM ses sid of 
                                        Just es -> set2list es 
                                        Nothing -> error "no hay empleados en dicho sector" 
todosLosCUIL :: Empresa -> [CUIL]
-- costo es o(E) 
todosLosCUIL (ConsE _ ce) = domM ce 

todosLosSectores :: Empresa -> [SectorId]
--Costo: O(S) donde en el peor caso S, es la cantidad total de claves 
todosLosSectores (ConsE ses ce) = domM ses 

agregarSector :: SectorId -> Empresa -> Empresa
-- Costo (log S) donde S es la cantidad total de claves
agregarSector sid (ConsE ses ce) = ConsE ( (assocM sid emptyS ses) ce )

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector sid c (ConsE ses ce) = let em = incorporarSector sid (consEmpleado c)
  Cons (agregarEmpleado sid em ses ) (assocM c em ce)

agregarEmpleado :: SectorId -> Empleado -> Map k v -> Map k v 
agregarEmpleado s e mp = case lookupM mp s of 
                        Just x -> addS e x  
                        Nothing -> error "no existe el sector id dado"


borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE ses ce) = let em = formJust(lookupM ce c)
                      ConsE (elSectorSinEmpleado (sectores em) em ses) (removeM c ce)

elSectorSinEmpleado :: SectorId -> Empleado ->  Map k v -> Map k v 
elSectorSinEmpleado sec e mp = case lookupM sec mp of
                            Just x -> removeS e x
                            Nothing -> error "no existe el sector id dado" 


data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)