
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

-- FALTA POKEMONES ---


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

splitHead :: [a] -> (a , [a])
splitHead 