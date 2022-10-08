import Map
map1 = ((assocM 10 6) ((assocM 5 7) ((assocM 5 6) emptyM)))

valuesM :: Eq k => Map k v -> [Maybe v]
-- Prop: obtiene los valores asociados a cada clave del map.
valuesM map = losValoresConClave (keys map) map 

losValoresConClave ::  Eq k => [k] -> Map k v -> [Maybe v]
losValoresConClave [] _ = []
losValoresConClave (k:ks) map = lookupM k map : losValoresConClave ks map 

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- Prop: indica si en el map se encuentran todas las claves dadas
todasAsociadas [] map = True 
todasAsociadas (k:ks) map = pertenece k (keys map) && todasAsociadas ks map

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = a == x || pertenece a xs

listToMap :: Eq k => [(k, v)] -> Map k v
-- Prop: convierte una lista de pares clave valor en un map.
listToMap [] = emptyM
listToMap (kv:kvs)  = assocM (fst(kv)) (snd(kv)) (listToMap kvs) 

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = lasClavesYvalorEnLista (keys map) map 

lasClavesYvalorEnLista :: Eq k => [k] -> Map k v -> [(k,v)]
lasClavesYvalorEnLista [] _ = []
lasClavesYvalorEnLista (k:ks) map = elValorDeK k map : lasClavesYvalorEnLista ks map 

elValorDeK :: Eq k => k -> Map k v -> (k,v)
elValorDeK k map = case lookupM k map of 
                    Just v -> (k,v)
                    Nothing -> error "NO EXISTE EL VALOR"


agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- Prop: dada una lista de pares clave valor, agrupa los valores
-- de los pares que compartan la misma clave.
agruparEq [] = emptyM
agruparEq ((k,v):kvs) = case lookupM k (agruparEq kvs) of
                        Just vs -> assocM k (v:vs) (agruparEq kvs)
                        Nothing -> assocM k (v:[]) (agruparEq kvs)

incrementar :: Eq k => [k] -> Map k Integer -> Map k Integer
-- Prop: dada una lista de claves de tipo k y 
-- un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar [] map = emptyM
incrementar (k:ks) map = case lookupM k map of 
                         Just i -> assocM k (i + 1) (incrementar ks map )
                         Nothing -> error "NO EXISTE LA CLAVE"

indexar :: [a] -> Map Int a
-- Prop: dada una lista de elementos construye un map 
--que relaciona cada elemento con su posición en la lista.



ocurrencias :: String -> Map Char Int
--Prop: dado un string, devuelve un map donde las claves son 
--los caracteres que aparecen en el string, y los valores la cantidad 
--de veces que aparecen en el mismo.
ocurrencias [] = emptyM
ocurrencias (s:sd) = assocM s ((apariciones s sd) + 1 ) (ocurrencias sd)  

apariciones :: Eq a => a -> [a] -> Int
apariciones  a []  = 0 
apariciones a (x:xs)  =  unoSi( a == x ) + apariciones a xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

