sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
longitud [] = 0 
longitud (n:ns) = 1 + longitud ns

sucesores :: [Int] -> [Int]
sucesores [] = [] 
sucesores (n:ns) = n+1 : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs 

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = a == x || pertenece a xs

apariciones :: Eq a => a -> [a] -> Int
apariciones  a []  = 0 
apariciones a (x:xs)  =  unoSi( a == x ) + apariciones a xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA a [] = []
losMenoresA a (x:xs) = if x < a
                       then  x : losMenoresA a xs
                       else losMenoresA a xs 


lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA a [] = []
lasDeLongitudMayorA a (xs:xss) = if longitud xs > a 
                                then xs : lasDeLongitudMayorA a xss
                                else lasDeLongitudMayorA a xss

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a = [a]
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar xs [] = xs
agregar (x:xs) ys = x : agregar xs ys 

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x 


elMinimo :: Ord a => [a] -> a
elMinimo    []     = error "la lista no puede ser vacia"
elMinimo (x:[]) = x
elMinimo (x:xs) = if x < elMinimo xs
                    then x
                    else elMinimo xs


-- RECURSION SOBRE NUMEROS --

factorial :: Int -> Int
-- Precond n es positivo
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n x = x : repetir (n-1) x 

losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 
-- x es mi primer elemento, y lo agrego a la lista que resta 


sinLosPrimeros :: Int -> [a] -> [a] 
sinLosPrimeros 0  ls = ls -- Si tengo 0, y una lista devuelvo esa lista
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

-- REGISTROS 

data Persona = P String Int 
   deriving Show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

mayoresA :: Int -> [Persona] -> [Persona]
-- Precond n es mayor a 0
mayoresA _ [] = []
mayoresA n (p:ps) = if edad p > n
                    then p : mayoresA n ps
                    else mayoresA n ps -- Descarto la primer persona, pq no cumple la condicion

promedioEdad :: [Persona] -> Int
-- Precond la lista posee al menos una persona
promedioEdad ps = div (sumarEdades ps) (longitud ps)
                    
sumarEdades :: [Persona] -> Int 
sumarEdades [] = 0
sumarEdades (p:ps) = edad p + sumarEdades ps 
                    -- Ya pido la edad, edad p

elMasViejo :: [Persona] -> Persona
-- Precond la lista posee al menos una persona
elMasViejo (p:ps) = if edad p > edad (elMasViejo ps)
                    then p
                    else elMasViejo ps


data TipoDePokemon = Agua | Fuego | Planta
  deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
  deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
  deriving Show

sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool                                               
sonDelMismoTipo Fuego Fuego = True
sonDelMismoTipo Planta Planta = True
sonDelMismoTipo Agua Agua = True
sonDelMismoTipo _ _ = False

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (ConsPokemon t _) = t 

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ p) = longitud p

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (ConsEntrenador _ pks) = cantPokemonsDeTipo tp pks

cantPokemonsDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonsDeTipo t [] = 0
cantPokemonsDeTipo t (p:ps)  = unoSi (sonDelMismoTipo t (tipoDePokemon p)) + cantPokemonsDeTipo t ps
                             
superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon Agua _)  (ConsPokemon Fuego _) = True
superaA (ConsPokemon Fuego _)  (ConsPokemon Planta _) = True
superaA (ConsPokemon Planta _)  (ConsPokemon Agua _) = True
superaA (ConsPokemon _ _)  (ConsPokemon _ _) = False
{-
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t (ConsEntrenador _ []) (ConsEntrenador _ _) = 0 --cuando la lista de pokemones del primer entrenador llegue a estar vacía
losQueLeGanan t (ConsEntrenador _ (pk1:pks1)) (ConsEntrenador _ pks2) = unoSi (leGanaATodos pk1 pks2)
                                                                     + losQueLeGanan (t (ConsEntrenador _ (pk1:pks1))
                                                                                       (ConsEntrenador _ pks2))
    -}
leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos pk1 [] = True -- CASO BORDE 
leGanaATodos pk1 (pk2:pks2) = (superaA pk1 pk2) -- el pokemon recibido le gana al primero de la lista
                              && (leGanaATodos pk1 pks2)

esMaestroPokemon :: Entrenador -> Bool 
esMaestroPokemon  (ConsEntrenador _ pks) = hayUnPokemonDeTipo pks Agua && 
                                          hayUnPokemonDeTipo pks Planta && 
                                          hayUnPokemonDeTipo pks Fuego

hayUnPokemonDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
hayUnPokemonDeTipo [] _ = False
hayUnPokemonDeTipo (pk:pks) tp =  sonDelMismoTipo (tipoDePokemon pk) tp || hayUnPokemonDeTipo pks tp  
                            -- sigo con la recursion porque mi primer pk no cumple la cond 

data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto = ConsProyecto String
      deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
      deriving Show
data Empresa = ConsEmpresa [Rol]
      deriving Show

empresa1 = ConsEmpresa [Developer Junior (ConsProyecto "Linuz"), Management Senior (ConsProyecto "xp")] 
empresa2 = ConsEmpresa [Management Senior (ConsProyecto "xp")]

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa  r) = sinRepetidos (losProyectos r)

losProyectos :: [Rol] -> [Proyecto]
losProyectos [] = []
losProyectos (r:rs)  = proyectoDeRol r : losProyectos rs

proyectoDeRol :: Rol -> Proyecto 
proyectoDeRol (Developer _ pro) = pro 
proyectoDeRol (Management _ pro) = pro 


sinRepetidos :: [Proyecto] -> [Proyecto]
sinRepetidos  [] = []
sinRepetidos (p:ps) = if elProyectoPertence p (sinRepetidos ps)
                        then  sinRepetidos ps
                        else p : sinRepetidos ps

elProyectoPertence :: Proyecto -> [Proyecto] -> Bool
elProyectoPertence  x []    = False
elProyectoPertence x (p:ps) = esElMismoProyecto x p || elProyectoPertence x ps
                              
esElMismoProyecto :: Proyecto -> Proyecto -> Bool
esElMismoProyecto (ConsProyecto p1) (ConsProyecto p2) = p1 == p2 

{-Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro-}
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa  r) []     = ..
losDevSenior (ConsEmpresa  r) (p:ps) = losQueSonDesarroladores r 



