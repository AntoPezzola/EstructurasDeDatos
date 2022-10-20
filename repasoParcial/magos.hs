

data EscuelaDeMagia = EDM 
(Set Hechizo)   -- todos los de la escuela enseñados
(Map Nombre Mago)  -- los magos de la escuela, con su nombre
(PriorityQueue Mago) -- los magos ordenados por rango, es decir cant de hechizos aprendidos

-- un mago aprende hechizos, y puede infornanos su nombre y los hechi que cnoce

  {- INV. rep
     - Si un mago conoce un hechizco, el mismo debe estar en los hechizos de la escuela 
     - El map que contiene los magos con sus nombres, son los mismos elementos que la pqMagos .. --mismos hechizos tmbn
     - en la PQ no hay magos repetidos.
 -}
 
fundarEscuela :: EscuelaDeMagia
 -- costo O(1), ya que el peor caso es de costo O(1)
fundarEscuela = EDM (emptyS) (emptyM) (emptyPQ)

estaVacia :: EscuelaDeMagia -> Bool
-- costo O(1), ya que isEmptyPQ es O(1) 
estaVacia EDM (_ _ ms) = isEmptyPQ ms 

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- Prop: Incorpora un mago a la escuela (si ya existe no hace nada). 
registrar n (EDM h nm m) = case lookupM n nm of   -- tengo que ver si esta, porque si el mismo esta no hace nada.
                          Nothing -> let mago = crearM n 
                                     in EDM h (assocM n mago nm) (insertPQ mago m)
                          Just _ -> EDM h nm m

magos :: EscuelaDeMagia -> [Nombre]
--Propósito:Devuelve los nombres de los magos registrados en la escuela.
-- costo O(M) ya que domM es costo constante
magos (EDM _ nm _) = domM nm 

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
--Prop: Devuelve los hechizos que conoce un mago dado.
--Precon: Existe un mago con dicho nombre.
-- costo O(log M) ya que hechizos tiene un costo de O(1) y lookupM O(log M ) 
hechizosDe n (EDM _ nm _) = case lookupM n nm of 
                            Just x -> hechizos x 
                            Nothing -> error "debe existir el mago" 

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Prop: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precon: Existe un mago con dicho nombre.
-- costo O(log M) ya que el lookupM tiene costo O(log M) y sizeS O(1)
leFaltanAprender n (EDM h nm _) = case lookupM n nm of 
                                 Just m -> sizeS(h) - sizeS(hechizos m)
                                Nothing -> error "debe existir el mago"

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
--Prop: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- Precon: Hay al menos un mago.
-- costo O(log M), ya que deleteM tiene costo (log M) + deleteMaxPQ con costo (log M)
egresarUno (EDM h nm m) = let mago = maxPQ m 
                          in (mago , EDM h (deleteM (nombre mago) nm) (deleteMaxPQ m))  
                        

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- Prop: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Precon: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H)
enseñar h n (EDM sh nm m) = let mago = fromJust(lookupM n nm) -- duda 
                           in EDM (añadirEnCasoQueNoExiste h sh) (assocM n (aprender h mago nm) (actualizarPQ mago m ))

actualizarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago
-- Costo (M log M) 
actualizarPQ m PQm = if m == maxPQ
                     then insertPQ m (deleteMaxPQ PQm)
                     else insertPQ maxPQ (actualizarPQ m (deleteMaxPQ PQm))

