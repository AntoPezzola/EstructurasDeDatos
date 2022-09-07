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
aplanar (a:as) = a ++ aplanar as

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
reversa (x:xs) = agregarAlFinal xs x 

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
--sinLosPrimeros 0  (x:xs) = xs -- Si tengo 0, y una lista devuelvo esa lista
sinlosPrimeros _ []     = []
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
cantPokemonsDeTipo t (p:ps)  = if sonDelMismoTipo t tipoDePokemon p 
                               then 1 + cantPokemonDe t (ConsEntrenador _ ps) -- error
                               else cantPokemonDe t (ConsEntrenador _ ps) 

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tp entrenador1 entrenador2 = 
    lesGanan (delMismoTipo tp entrenador1) (pokemones entrenador2)

delMismoTipo :: TipoDePokemon -> Entrenador -> [Pokemon]
delMismoTipo tp (ConsEntrenador n (pk:pks)) = if (sonDelMismoTipo tp pk)
                                              then pk : delMismoTipo tp (ConsEntrenador n pks)
                                              else delMismoTipo tp (ConsEntrenador n pks)


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ pks) = tienePokemonesDeTodosLosTipos pks

tienePokemonesDeTodosLosTipos :: [Pokemon] -> Bool
tienePokemonesDeTodosLosTipos [] = False
tienePokemonesDeTodosLosTipos (pks) =  hayUnPokemonDeTipo pks Agua &&
                                         hayUnPokemonDeTipo pks Fuego &&
                                         hayUnPokemonDeTipo pks Planta

hayUnPokemonDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
hayUnPokemonDeTipo [] _ = False
hayUnPokemonDeTipo (pk:pks) tp =  sonDelMismoTipo pk tp || hayUnPokemonDeTipo pks tp  
                            -- sigo con la recursion porque mi primer pk no cumple la cond 