import Persona
import PriorityQueue 

ff       = crearPersona "Fidel"     "ML"      53
ale      = crearPersona "Alejandro" "Castro"  36
cristian = crearPersona "Cristian"  "Sottile" 29
juan     = crearPersona "Juan"      "Plotuki" 29


instance Eq Persona where 
 p1 == p2 = edad p1 == edad p2 
-- === DUDA ====
instance Ord Persona where
 p1 <= p2 =  edad p1 <= edad p2 

instance Show Persona where
  show p = "Persona { nombre <- "   ++ show (nombre p)
                ++ ", apellido <- " ++ show (apellido p)
                ++ ", edad <- "     ++ show (edad p)
                ++ " }"

crearPersona :: String -> String -> Int -> Persona
crearPersona n a e = crecerVeces e (nacer n a)

crecerVeces :: Int -> Persona -> Persona
crecerVeces 0 p = p
crecerVeces n p = crecer (crecerVeces (n-1) p)

ejPQ = insertPQ ff
     $ insertPQ ale
     $ insertPQ cristian
     $ emptyPQ  

     