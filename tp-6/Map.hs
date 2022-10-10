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


module Map 
  (Map, emptyM , assocM , lookupM , deleteM , keys) 
  where 

data Map k v = M [(k,v)]

emptyM :: Map k v
assocM :: Eq k => k -> v -> Map k v -> Map k v
lookupM :: Eq k => k -> Map k v -> Maybe v
deleteM :: Eq k => k -> Map k v -> Map k v
keys :: Map k v -> [k]


emptyM   = M []
assocM key valor (M xs) =  M (asociar k v kvs) 
-- PROP.: describe el map dado donde la clave dada se asocia
  --        al valor dado (si estaba asociada a otra cosa, solamente
  --        vale la última asociación)
lookupM key (M xs) = valorConClave key xs 
-- PROP.: describe el valor asociado a la clave en el map 
  --        si existe, o Nothing, si no
deleteM key (M xs) = M (borrar key xs) 
 -- PROP.: describe el map dado, pero donde la clave dada no 
  --        se asocia a ningún valor
keys (M xs) = claves xs 
  -- PROP.: describe la lista de todas las claves definidas
  --        en el map, sin repetidos (el dominio del map)
  
valorConClave :: Eq k => k -> [(k,v)] -> Maybe v 
valorConClave  _ [] = Nothing 
valorConClave key (kv:kvs) = if key == fst(kv) 
                             then Just snd(kv)
                             else valorConClave key kvs 

borrar :: Eq k => k -> [(k,v)] -> [(k,v)]

asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [ (k,v) ]
asociar k v ((k',v'):kvs) = if k==k' 
                            then (k',v):kvs 
                            else (k', v') : asociar k v kvs