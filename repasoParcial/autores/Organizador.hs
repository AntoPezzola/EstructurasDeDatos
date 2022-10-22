
module Organizador (Organizador ,  nuevo , agregarPrograma, todosLosProgramas 
autoresDe, programasDe ,  programaronJuntas , nroProgramasDePersona)

{-INV.REP:
     -  Los codigos los códigos identificadores de programas del organizador, estan
     en ambos map
       -}

where 
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

nuevo :: Organizador
nuevo = MkO emptyM emptyM

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MK0 cst pst) c sp= MkO (assocM c sp cst) 

todosLosProgramas :: Organizador -> [Checksum]
--Prop:denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (MK0 cst pst) = domM(cst) -- por inv se que todos los cod que esten aca, estan en el otro map

autoresDe :: Organizador -> Checksum -> Set Persona
--Prop: denota el conjunto de autores que aparecen en un programa determinado.
--Precon: el Checksum debe corresponder a un programa del organizador.
-- O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (MK0 cst _ ) ch = case lookupM cst ch of
                           Just ps -> ps 
                           Nothing -> error "el checksum debe corresponder a un org"

programasDe :: Organizador -> Persona -> Set Checksum
--Prop:denota el conjunto de programas en los que participó una determinada persona.
--Precon: la persona debe existir en el organizador.
-- O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (MK0 _ pst) p = case lookupM pst p of 
                            Just c -> C
                            Nothing -> error "debe existir la persona"

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Prop:dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precon: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, 
--donde P es la cantidad de personas distintas que aparecen en todos los
--programas del organizador, y C la cantidad total de programas.
programaronJuntas (MK0 _ pst) p1 p2 = isEmpty(intersection (softwareDe p1 pst) (softwareDe p2 pst)) -- duda e inv.rep

softwareDe :: Persona -> Map k v -> Set Checksum
softwareDe p mp = case lookupM p mp of 
                  Just ps -> ps 
                  Nothing -> emptyS 
                   
nroProgramasDePersona :: Organizador -> Persona -> Int
--Prop: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
--O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona (MK0 _ pst) p = case lookupM p pst of 
                                      Just pro -> sizeS(pro)
                                      Nothing -> 0
{- DUDAS 
    el inv de representacion es el correcto 
    como negar el isEmpty-}