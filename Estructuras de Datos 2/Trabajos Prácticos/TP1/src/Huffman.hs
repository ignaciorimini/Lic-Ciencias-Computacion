module Huffman where

import Data.Map as DM (Map, fromList, insertWith, toList)
import Heap

{-
Integrantes grupo:
- Ignacio Rímini
-}

-- Bits y códigos
data Bit = Zero | One deriving (Eq, Show)
type Code = [Bit]

-- Árbol de codificación
data HTree = Leaf Char Int | Node HTree HTree Int deriving Show

-- Función que devuelve el peso de un árbol HTree.
weight :: HTree -> Int
weight (Leaf _ w)   = w
weight (Node _ _ w) = w

-- Diccionarios de frecuencias y códigos.
-- FreqMap es un diccionario que asocia cada caracter con su cantidad de apariciones en un string.
-- CodeMap es un diccionario que asocia cada caracter con su código binario en forma de lista de bits [Bit].
type FreqMap = Map Char Int
type CodeMap = Map Char Code


----------------------------------------
-- Ejercicio 1: Dar una instancia de la clase Ord para el tipo HTree. Para ello, primero debe dar una instancia de la clase Eq.

-- Dos árboles serán iguales si tienen el mismo peso.
instance Eq HTree where
    t1 == t2 = weight t1 == weight t2

-- Un árbol es menor que otro si su peso es menor.
instance Ord HTree where
    compare t1 t2 = compare (weight t1) (weight t2)


----------------------------------------
-- Ejercicio 2.

-- insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
-- Función que inserta un par (clave, valor) a un Map.
-- Si la clave ya existe, combina el nuevo valor con el anterior usando la función pasada como argumento.
-- Si no existe, la inserta con el valor dado.

-- Usamos insertWith con la función combinadora de la suma y el valor 1, de tal modo que si la clave ya existía con un valor numérico x, le sume 1 a tal valor, quedando (c, x+1) en el nuevo mapa.

buildFreqMap :: String -> FreqMap
buildFreqMap [] = DM.fromList []
buildFreqMap (c:cs) = DM.insertWith (+) c 1 (buildFreqMap cs)


----------------------------------------
-- Ejercicio 3.

-- Esta función es la principal para construir un árbol de Huffman a partir de un mapa de frecuencia de aparición de caracteres (Char, Int).
-- Su funcionamiento consiste en convertir el mapa de frecuencias en una lista [(Char, Int)], pasar dicha lista a la función buildInitialHeap que construye el min-heap inicial con todas hojas y luego construir el árbol de huffman definitivo con la función buildHTreeFromHeap.
buildHTree :: FreqMap -> HTree
buildHTree freqMap = 
    let initialHeap = buildInitialHeap (DM.toList freqMap)
    in buildHTreeFromHeap initialHeap

-- Esta función recibe una lista [(Char, Int)] que indica el caracter y su frecuencia de aparición en el string y construye un min-heap inicial convirtiendo a cada par (c,v) en una hoja de un HTree e insertándolos en el heap.
-- De esta manera, la salida será un min-heap cuyos elementos serán hojas de un HTree y donde el nodo raíz será el caracter con menor cantidad de apariciones.
buildInitialHeap :: [(Char, Int)] -> Heap HTree
buildInitialHeap [] = empty
buildInitialHeap ((c,v):xs) = insert (Leaf c v) (buildInitialHeap xs)

-- Esta función recibe un heap de HTrees y construye el árbol de huffman.
-- La función extrae los dos árboles con menor frecuencia (menor peso), los combina en un nuevo nodo, y reinserta el nodo combinado con el heap.
buildHTreeFromHeap :: Heap HTree -> HTree
buildHTreeFromHeap heap 
    | isEmpty heap = error "Error: Heap vacío"
    | isEmpty (deleteMin heap) = findMin heap -- Solo queda un árbol.
    | otherwise =
        -- Tomamos el mínimo, lo borramos y volvemos a tomar el mínimo, para tener los dos elementos mínimos del heap. Los combinamos en un HTree y volvemos a insertar dicho HTree en el heap luego de eliminar los dos mínimos.
        let t1 = findMin heap
            h1 = deleteMin heap
            t2 = findMin h1
            h2 = deleteMin h1
            nuevo = Node t1 t2 (weight t1 + weight t2)
        in buildHTreeFromHeap (insert nuevo h2)


----------------------------------------
-- Ejercicio 4

buildCodeMap :: HTree -> CodeMap
buildCodeMap = undefined


----------------------------------------
-- Ejercicio 5

encode :: CodeMap -> String -> Code
encode = undefined


----------------------------------------
-- Ejercicio 6

decode :: HTree -> Code -> String
decode = undefined


----------------------------------------
-- Ejercicio 7

engFM :: FreqMap
engFM = fromList [
    ('a', 691),
    ('b', 126),
    ('c', 235),
    ('d', 360),
    ('e', 1074),
    ('f', 188),
    ('g', 170),
    ('h', 515),
    ('i', 589),
    ('j', 13),
    ('k', 65),
    ('l', 340),
    ('m', 203),
    ('n', 571),
    ('o', 635),
    ('p', 163),
    ('q', 8),
    ('r', 506),
    ('s', 535),
    ('t', 766),
    ('u', 233),
    ('v', 83),
    ('w', 200),
    ('x', 13),
    ('y', 167),
    ('z', 6),
    (' ', 1370),
    (',', 84),
    ('.', 89)
    ]
