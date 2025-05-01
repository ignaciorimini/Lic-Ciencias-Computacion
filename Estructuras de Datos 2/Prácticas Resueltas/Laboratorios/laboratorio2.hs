
module Lab02 where

{-
   Laboratorio 2
   EDyAII 2022
-}

import Data.List

-------------------------------------------------------------------
-- 1) Dada la siguiente definición para representar árboles binarios:
data BTree a = E | Leaf a | Node (BTree a) (BTree a) deriving Show

-- Definir las siguientes funciones:
-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E = -1
altura (Leaf _) = 0
altura (Node l r) = max (altura l) (altura r) + 1

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si cada nodo tiene 0 o 2 hijos
-- y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto (Leaf _) = True
perfecto (Node l r) = perfecto l && perfecto r && (altura l == altura r)

-- c) inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E = []
inorder (Leaf v) = [v]
inorder (Node l r) = inorder l ++ inorder r 


-------------------------------------------------------------------
-- 2) Dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):

-- data GTree a = EG | NodeG a [GTree a]

-- data BinTree a = EB | NodeB (BinTree a) a (BinTree a)

{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
   el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
   de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).
   
   
   Por ejemplo, sea t: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt t =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}

data GTree a = EG | NodeG a [GTree a] deriving Show

data BinTree a = EB | NodeB (BinTree a) a (BinTree a) deriving Show

-- Esta es la función principal. Recibe un árbol general (GTree a) y devuelve el árbol binario correspondiente (BTree a).
-- Internamente llama a g2bt' pasando el árbol general y una lista vacía de hermanos.
g2bt :: GTree a -> BinTree a
g2bt t = g2bt' t []

-- Recibe dos parámetros: un nodo actual del árbol general y una lista de hermanos del nodo actual (los nodos que están a su derecha).
-- El comportamiento depende de si el nodo tiene hijos y/o hermanos:
-- 1. Si no tiene hijos ni hermanos, crea un nodo binario sin hijos.
-- 2. Si tiene hijos pero no hermanos, su hijo izquierdo es su primer hijo convertido.
-- 3. Si tiene hijos y hermanos, su hijo izquierdo es su primer hijo convertido y su hijo derecho es el primer hermano convertido.
-- 4. Si no tiene hijos pero sí hermanos, su hijo izquierdo es vacío y su hijo derecho es el primer hermano convertido.
g2bt' :: GTree a -> [GTree a] -> BinTree a
g2bt' EG _ = EB
g2bt' (NodeG a []) [] = NodeB EB a EB -- Sin hijos ni hermanos.
g2bt' (NodeG a (t:ts)) [] = NodeB (g2bt' t ts) a EB -- Con hijos sin hermanos.
g2bt' (NodeG a (t:ts)) (h:hs) = NodeB (g2bt' t ts) a (g2bt' h hs) -- Con hijos y hermanos.
g2bt' (NodeG a []) (h:hs) = NodeB EB a (g2bt' h hs) -- Sin hijos con hermanos.

-- Ejemplo: g2bt (NodeG 'A' [(NodeG 'B' []),(NodeG 'C' []),(NodeG 'D' []),(NodeG 'E' [])])


-------------------------------------------------------------------
-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo 
      que contenga la máxima cantidad de elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6
                             
      dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima
      cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
   -}

-- La función dcn utiliza levels para obtener todos los niveles de un árbol binario, y luego aplica la función buscar para encontrar el nivel más profundo que tenga la cantidad máxima posible de nodos (2 elevado al número de nivel). 
-- La función zip asocia a cada nivel su número (nivel 0, nivel 1, nivel 2, etc.) y se llama a buscar con esa lista y una lista que va almacenando el nivel de mayor profundidad que esté completo.
dcn :: BinTree a -> [a]
dcn t = buscar (zip [0..] (levels t)) []
   where
      buscar [] xs = xs
      buscar ((n,xs):rest) ts = 
         if length xs == 2^n
            then buscar rest xs
            else buscar rest ts

-- La función levels toma un árbol binario y devuelve una lista de listas, donde cada sublista representa los elementos de un nivel del árbol, empezando por el nivel 0 (la raíz). 
-- Por ejemplo, para un árbol donde el primer nivel tiene el elemento 1, el segundo nivel tiene 2 y 3, y el tercero tiene 4, 5 y 6, levels devuelve [[1], [2,3], [4,5,6]]. 
levels :: BinTree a -> [[a]]
levels EB = []
levels (NodeB l a r) = [a] : mergeLevels (levels l) (levels r)

-- La función mergeLevels recibe dos listas de niveles (listas de listas) y las combina de manera que los elementos de cada nivel correspondiente se unan.
-- Si un árbol tiene niveles más profundos que el otro, los niveles extra simplemente se agregan al final. 
-- Esta función es usada en levels para combinar los niveles provenientes del subárbol izquierdo y derecho.
mergeLevels :: [[a]] -> [[a]] -> [[a]]
mergeLevels [] ys = ys
mergeLevels xs [] = xs
mergeLevels (x:xs) (y:ys) = (x ++ y) : mergeLevels xs ys

-- Ejemplos y casos de prueba.
arbolEjemplo = NodeB EB 1 (NodeB EB 2 (NodeB EB 3 EB))
arbolEjemplo2 = NodeB (NodeB EB 2 (NodeB EB 4 EB)) 1 (NodeB (NodeB EB 5 EB) 3 (NodeB EB 6 EB)) -- dcn debería devolver [2,3]
arbolEjemplo3 = NodeB (NodeB (NodeB EB 1 EB) 2 (NodeB EB 4 EB)) 3 (NodeB (NodeB EB 5 EB) 7 (NodeB EB 6 EB)) -- dcn debería devolver [1,4,5,6]


{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2 -}

maxn :: BinTree a -> Int
maxn t = buscarNivel (zip [0..] (levels t)) 0
   where
      buscarNivel [] n = n
      buscarNivel ((i,xs):rest) n =
         if length xs == 2^i
            then buscarNivel rest (i+1)
            else buscarNivel rest n

{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar :: BinTree a -> BinTree a
podar t = podar' t (maxn t)

podar' :: BinTree a -> Int -> BinTree a
podar' _ 0 = EB
podar' EB _ = EB
podar' (NodeB l a r) n = NodeB (podar' l (n-1)) a (podar' r (n-1))