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
maxDelPar (x, y) = max x y 

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
vieneDespues Lunes Martes = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True
vieneDespues Domingo Lunes = True
vieneDespues d e = False

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
implica True True = True
implica True False = False
implica False False = True
implica False True = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien d e = False

oBien :: Bool -> Bool -> Bool
oBien True False = True
oBien True True = True
oBien False True = True
oBien d e = False

