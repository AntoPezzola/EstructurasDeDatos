module Map 
  (Map, emptyM , assocM , lookupM , deleteM , keys) 
  where 

data Map k v = M [(k,v)]
  deriving Show 

emptyM :: Map k v
assocM :: Eq k => k -> v -> Map k v -> Map k v
lookupM :: Eq k => k -> Map k v -> Maybe v
deleteM :: Eq k => k -> Map k v -> Map k v
keys :: Map k v -> [k]


emptyM   = M []
assocM key valor (M xs) =  M ((key,valor) : xs)
lookupM key (M xs) = valorConClave key xs 
deleteM key (M xs) = M (borrar key xs) -- Prop: borra una asociación dada una clave
keys (M xs) = claves xs 

valorConClave :: Eq k => k -> [(k,v)] -> Maybe v 
valorConClave  _ [] = Nothing 
valorConClave key (kv:kvs) = if key == fst(kv) 
                             then Just (snd(kv))
                             else valorConClave key kvs 

borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar _ [] = []
borrar key (kv:kvs) = if key == fst(kv) 
                      then borrar key kvs 
                      else kv : borrar key kvs  

claves :: [(k,v)] -> [k]
claves [] = []
claves (kv:kvs) = fst(kv) : claves kvs 



