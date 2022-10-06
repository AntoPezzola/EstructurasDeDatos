module Persona
  (Persona, nacer, edad, nombre, apellido, nombreCompleto, crecer)
 where

data Persona = P String String Int
  {- INV.REP.:
      * el nombre y el apellido no son vacíos y no contienen espacios
      * la edad es >= 0
  -}

nacer          :: String -> String -> Persona -- PRECOND: * el nombre y el apellido no son vacíos y no contienen espacios
edad           :: Persona -> Int
nombre         :: Persona -> String
apellido       :: Persona -> String
nombreCompleto :: Persona -> String
crecer         :: Persona -> Persona

nacer          n a       = 
    if not (esNombreValido n)
     then error "El nombre no es adecuado"
     else if not (esApellidoValido a)
           then error "El apellido no es adecuado" 
           else P n a 0
edad           (P _ _ e) = e
nombre         (P n _ _) = n
apellido       (P _ a _) = a
nombreCompleto (P n a _) = n ++ " " ++ a
crecer         (P n a e) = P n a (e+1)

esNombreValido     n = noVacioSinEspacios n
esApellidoValido   a = noVacioSinEspacios a
noVacioSinEspacios s = s/="" && not (elem ' ' s)

