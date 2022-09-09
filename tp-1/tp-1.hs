sucesor :: Int -> Int
-- Dado un número devuelve su sucesor
sucesor x = x+1 

sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
--Dado dos números, devuelve un par donde la primera componente es la división del
--primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
--para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
--provista por Haskell.
divisionYResto x y = (div x y , mod x y)

maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.
maxDelPar (x, y) =  if x > y
                    then x
                    else  y 

-- TIPOS NUMERICOS
data Dir = Este | Norte | Sur | Oeste
    deriving Show

opuesto :: Dir -> Dir
--Dada una dirección devuelve su opuesta
opuesto Este = Oeste
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Oeste = Este


iguales :: Dir -> Dir -> Bool
iguales Este Este = True
iguales Sur Sur = True
iguales Norte Norte = True
iguales Oeste Oeste = True
iguales _ _ = False

-------

siguiente :: Dir -> Dir
--Dada una dirección devuelve la siguiente. Es una funcion total
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

-- DIAS DE SEMANA ----

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM d = False 

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = numeroDia dia1 > numeroDia dia2

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes = 1 
numeroDia Martes = 2
numeroDia Miercoles = 3
numeroDia Jueves = 4 
numeroDia Viernes = 5
numeroDia Sabado =  6
numeroDia Domingo = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Martes = True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves = True
estaEnElMedio Viernes = True
estaEnElMedio Sabado = True
estaEnElMedio d = False

-- BOOLEANOS --

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True a = a
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True e = e
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien False a = a
oBien _ _ = True



-- REGISTROS--

data Persona = P String Int 
   deriving Show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = (P n (e + 1)) 

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre  nom (P n e) = (P nom e) 

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n1 e1) (P n2 e2) = if( e1 > e2) 
                                    then (P n1 e1)
                                    else (P n2 e2)

                                    
data Pokemon = P TipoDePokemon Int
        deriving Show
data Entrenador = E String Pokemon Pokemon
        deriving Show
data TipoDePokemon = Agua | Fuego | Planta
  deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pkmn t e) (Pkmn t1 e1) =  primeroSuperaASegundo t t1

primeroSuperaASegundo:: TipoDePokemon -> TipoDePokemon -> Bool
primeroSuperaASegundo Agua Fuego = True
primeroSuperaASegundo Fuego Planta = True
primeroSuperaASegundo Planta Agua = True
primeroSuperaASegundo t1 t2 = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (E _ Pk1 Pk2) (E _ Pk1 Pk2) =  juntarPokemon (E _ Pk1 Pk2) (E _ Pk1 Pk2)

-- FUNCIONES POLIMORFICAS -- 

loMismo :: a -> a 
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

{- ¿Por qué estas funciones son polimórficas?
 Porque utilizamos una sola definicion que opera sobre muchos tipos y definen estructuras de
 datos genéricas -}

-- PATTERN MATCHING SOBRE LISTAS --

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia (_:_) = False

elPrimero :: [a] -> a
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])  
--PRECONDICION: la lista dada no debe estar vacía
splitHead a = (elPrimero a, sinElPrimero a).
