data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

arbol1 = NodeT 10 (NodeT 7 EmptyT EmptyT) (NodeT 20 EmptyT EmptyT) 

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _  EmptyT = False
belongsBST y (NodeT x ti td ) = if (x==y) then True
                                else if (x > y)
                                     then belongsBST y ti 
                                else belongsBST y td 

Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
--Prop:construye una empresa con la información de empleados dada.Los sectores no tienen empleados.
comenzarCon rs cs = agregarSectores rs (agregarEmpleadosSinSectores cs consEmpresa)

agregarEmpleadosSinSectores :: [CUIL] -> Empresa -> Empresa
agregarEmpleadosSinSectores [] e     = e
agregarEmpleadosSinSectores (c:cs) e = agregarEmpleado [] c (agregarEmpleadosSinSectores cs e) 

agregarSectores :: [SectorId] -> Empresa -> Empresa
agregarSectores [] e     = e
agregarSectores (r:rs) e = agregarSectores rs (agregarSector r e) 


recorteDePersonal :: Empresa -> Empresa
--Prop:dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
recorteDePersonal e =  sinLaMitad(todosLosCUIL e) e 

sinLaMitad :: [CUIL] -> Empresa -> Empresa
sinLaMitad [] e = e 
sinLaMitad cs e = let n = div (lenght cs)  2 in 
                   borrarCentEmpleados cs n e 

borrarCantEmpleados :: [CUIL] -> Int -> Empresa -> Empresa
borrarCantEmpleados [] _ e = e 
borrarCantEmpleados _  0 e = e  
borrarCantEmpleados (c:cs) n e = 
            borrarEmpleado c (borrarCantEmpleados cs (n-1) e)

convertirEnComodin :: CUIL -> Empresa -> Empresa
--Prop: dado un CUIL de empleado le asigna todos los sectores de la empresa.
convertirEnComodin c em = agregarATodosLosSectores c (todosLosSectores em) em 

agregarATodosLosSectores :: CUIL -> [SectorId] -> Empresa -> Empresa
agregarATodosLosSectores c [] em = em 
agregarATodosLosSectores c (s:sids) em = agregarASector s c (agregarATodosLosSectores c sids em)

esComodin :: CUIL -> Empresa -> Bool
--Prop: dado un CUIL de empleado indica si el empleado está en todos los sectores.
esComodin c em = 
                   


