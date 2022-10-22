import  Organizador

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
--Prop: dadas dos personas y un organizador, denota el conjunto de
-- aquellos programas en las que las personas programaron juntas.
programasEnComun p1 p2 or = intersection (programasDe p1) (programasDe p2)

esUnGranHacker :: Organizador -> Persona -> Bool
--Prop:denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.   
esUnGranHacker org p =  (nroProgramasDePersona org p) = length(todosLosProgamasDe org) 

