{-
INTEGRANTES: Buczek Federico y Rimini Ignacio.

Descripción: TTree k v es un árbol ternario donde cada nodo tiene un valor asociado y tres hijos: hijo izquierdo, hijo central e hijo derecho. 

Un nodo puede ser de 3 tipos diferentes:
 + Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v): representa un nodo con una clave k, un valor opcional v y tres hijos: izquierdo central y derecho.
 + Leaf k v: representa un nodo hoja con una clave k y un valor v.
 + E: representa un árbol vacío.

Definición de un tipo de datos TTree: 
 + arbol = Node clave (Just valor) hijoIzq hijoCentro hijoDer
 + hoja = Leaf clave valor
 + arbolVacio = E

-- Ejemplo diccionario: 
-- {("se", 8); ("si", 4); ("sin", 7); ("ras", 1); ("re", 16); ("red", 9); ("res", 4); ("reo", 2)}
-- arbol = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E) (Node 'o' (Just 2) (Leaf 'd' 9) E (Leaf 's' 4)) E) (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8) (Leaf 'n' 7) E) E)
-}
module TTree where

import Data.Maybe (isJust)

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E deriving Show

--------------------------------------
-- Esta función toma como entrada una lista de claves que son Strings y un árbol de búsqueda TTree, y devuelve el valor asociado a la clave, o Nothing si la clave no está en el árbol.
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search (x:xs) (Leaf c v)
    | x == c && null xs = Just v
    | otherwise = Nothing
search (x:xs) (Node c v arbol1 arbol2 arbol3)
    | x < c                     = search (x:xs) arbol1
    | x == c && null xs         = v
    | x == c && not(null xs)    = search xs arbol2
    | otherwise                 = search (x:xs) arbol3

-- Ejemplo diccionario: {("sa", 1); ("se", 2); ("sen", 5); ("si", 3)}
-- arbol2 = Node 's' Nothing E (Node 'e' (Just 2) (Leaf 'a' 1) (Leaf 'n' 5) (Leaf 'i' 3)) E
-- search "sa" arbol2 -> Just 1
-- search "se" arbol2 -> Just 2
-- search "si" arbol2 -> Just 3
-- search "sen" arbol2 -> Just 5


--------------------------------------
-- Agrega (clave, valor) a un TTree
-- si ya esta la clave, la actualiza
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v 
insert (x:xs) nv E =
    if null xs 
        then Leaf x nv
        else Node x Nothing E (insert xs nv E) E
insert (x:xs) nv (Leaf c v) =
    if  x == c && null xs 
        then Leaf x nv
        else insert (x:xs) nv (Node c (Just v) E E E) 
insert (x:xs) nv (Node c v l m r) 
    | x < c                     = Node c v (insert (x:xs) nv l) m r
    | x == c && null xs         = Node c (Just nv) l m r      
    | x == c                    = Node c v l (insert xs nv m) r
    | otherwise                 = Node c v l m (insert (x:xs) nv r)


--------------------------------------
-- Función que dado un árbol TTree, devuelve una lista ordenada con todas las claves del árbol.
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf c _) = [[c]]
keys (Node c v arbol1 arbol2 arbol3) = 
    let arbolIzq = keys arbol1
        arbolCentral = map (c:) (keys arbol2)
        arbolDer = keys arbol3
        caracterConcat = if isJust v then [[c]] else []
    in arbolIzq ++ caracterConcat ++ arbolCentral ++ arbolDer


--------------------------------------
-- borra el par (clave, valor) del arbol 
delete :: Ord k => [k] -> TTree k v -> TTree k v 
delete _ E = E 
delete key l@(Leaf c v) = 
    if length key == 1 && head key == c then E else l
delete k t =  
    let (raiz, nuevot) = deleteH k t
    in if raiz then nuevot else merge t


-- deleteH retorna: 
--   *raiz: es True si la raiz no se elimina false en caso contrario
--   *t: arbol luego de eliminar la palabra
-- internamente raiz (o prefijoc) es False mientras estamos en el sufijo unico de 
-- la palabra a eliminar y luego es True
-- se puede decir que es True cuando ya hicimos todos los cambios que se debian hacer (False cuando no)
deleteH :: Ord k => [k] -> TTree k v -> (Bool, TTree k v)
deleteH key E = (True, E)                  -- la palabra no estaba en el arbol (no hacemos cambio)
deleteH key l@(Leaf c _) = 
    if length key == 1 && head key == c    -- si encontramos el valor en una hoja
        then (False, E)                    -- borramos la hoja y prefijoc = False (puede ser que el sufijo unico sea mayor)
        else (True, l)                     -- la palabra no estaba en el arbol (no hacemos cambios)
deleteH (x:xs) t@(Node c v l m r) 
    | x < c = 
        let (prefijoc, nt) = deleteH (x:xs) l
        in if prefijoc 
            then (True, Node c v nt m r)        -- prefijo compartido, ya se elimino todo lo que se debia eliminar
            else (True, Node c v (merge l) m r) -- subarbol medio de l contiene el sufijo unico a [m] 
    | x == c && null xs = 
        (not (isE m), Node c Nothing l m r)     -- si m no es E, [k] es prefijo de otra y solo seteamos v en Nothing
    | x == c = 
        let 
            (prefijoc, nt) = deleteH xs m 
            p = isStart t                    
        in if prefijoc 
            then (True, Node c v l nt r)                      -- estamos en prefijo compartido
            else (p, if p then Node c v l (merge m) r else t) -- si p, el sufijo de m es unico a [k]
    | x > c = 
        let (prefijoc, nt) = deleteH (x:xs) r
        in if prefijoc 
            then (True, Node c v l m nt) 
            else (True, Node c v l m (merge r)) -- el subarbol medio de r con


-- isStart devuelve True si estamos en el nodo mas bajo
-- que es parte de un prefijo compartido
-- es decir el subarbol medio
isStart :: Ord k => TTree k v -> Bool 
isStart E = False 
isStart  (Leaf _ _) = False 
isStart (Node c v l m r) 
    | isJust v                      = True 
    | isE m  || isLeaf m            = False 
    | not (isE ml) || not (isE mr)  = True
    | otherwise                     = False 
        where (mc, mv, ml, mm, mr)  = getComp m 


-- Agarra el subarbol izquierdo y el subarbol derecho 
-- de un TTree valido y los convierte en uno
-- descarta el subarbol medio
merge :: Ord k => TTree k v -> TTree k v
merge E = E
merge (Leaf _ _) = E 
merge (Node _ _ l _ r)
    | isE l           = r 
    | isE r           = l
    | otherwise       = insertTree r l 


-- inserta el primer arbol en el segundo
-- sabiendo que la clave de la raiz del primero es mayor que la clave de la raiz del segundo
insertTree :: Ord k => TTree k v -> TTree k v -> TTree k v
insertTree r E = r 
insertTree r (Leaf c v) = Node c (Just v) E E r
insertTree r (Node c v ll lm lr) = Node c v ll lm (insertTree r lr) 


-- es un nodo el fin de una palabra
isEnd :: TTree k v -> Bool 
isEnd E = False 
isEnd (Leaf _ _) = True 
isEnd (Node _ v _ _ _) = isJust v

-- da los componentes de un nodo que no sea E ni una hoja
getComp :: TTree k v -> (k, Maybe v, TTree k v, TTree k v, TTree k v)
getComp (Node c v l m r) = (c, v, l, m, r)

-- determina si un nodo es E
isE :: Ord k => TTree k v -> Bool 
isE E = True 
isE t = False 

-- determina si un nodo es una hoja
isLeaf :: Ord k => TTree k v -> Bool 
isLeaf (Leaf _ _) = True 
isLeaf t = False 

-- consigue la clave de un nodo 
getK :: Ord k => TTree k v -> k 
getK (Leaf c v)  = c 
getK (Node c _ _ _ _) = c 

--------------------------------------
-- Dada una lista de (claves, valores)
-- retorna un TTree con ellos
createTTree :: Ord k => [([k], v)] -> TTree k v
createTTree [] = E
createTTree ((k, v): xs) = insert k v (createTTree xs)

-- borrar las claves en la lista del arbol t
delList :: Ord k => [[k]] -> TTree k v -> TTree k v
delList [] t = t
delList (x: xs) t = delete x (delList xs t)

-- Ejemplo diccionario: {("se", 8); ("si", 4); ("sin", 7); ("ras", 1); ("re", 16); ("red", 9); ("res", 4); ("reo", 2)}
t1 = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E) (Node 'o' (Just 2) (Leaf 'd' 9) E (Leaf 's' 4)) E) (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8) (Leaf 'n' 7) E) E)
-- keys arbol = ["ras","re","red","reo","res","se","si","sin"]

-- Ejemplo diccionario: {("sa", 1); ("se", 2); ("sen", 5); ("si", 3)}
t2 = Node 's' Nothing E (Node 'e' (Just 2) (Leaf 'a' 1) (Leaf 'n' 5) (Leaf 'i' 3)) E
-- keys arbol2 = ["sa","se","sen","si"]

l3 = [("se", 8), ("si", 4), ("sin", 7), ("ras", 1), ("re", 16), ("red", 9), ("res", 4), ("reo", 2)]
t3 = createTTree l3

l4 :: [(String, Integer)]
l4 = [("kastu", 11), ("kantante", 9), ("kanta", 8), ("fast", 7), ("arto", 6), ("dalto", 5), ("karta", 4), ("kasta", 3), ("iglu", 2), ("gato", 1)]
t4 = createTTree l4