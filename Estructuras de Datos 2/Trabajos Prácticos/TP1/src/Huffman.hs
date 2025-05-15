module Huffman where

import Data.Map as DM (Map, singleton, fromList, insertWith, toList, union, lookup)
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
-- Ejercicio 1:

-- Dos árboles serán iguales si tienen el mismo peso.
instance Eq HTree where
    t1 == t2 = weight t1 == weight t2

-- Un árbol es menor que otro si su peso es menor.
instance Ord HTree where
    compare t1 t2 = compare (weight t1) (weight t2)


----------------------------------------
-- Ejercicio 2.

-- Esta función recibe un string y computa la cantidad de apariciones de cada uno de sus caracteres.
-- Lo hace usando insertWith con la función combinadora de la suma y el valor 1, de tal modo que si la clave ya existía con un valor numérico x, le sume 1 a tal valor, quedando (c, x+1) en el nuevo mapa.
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
    | isEmpty heap = error "Error: Heap vacío."
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

-- Esta función construye un diccionario que mapea cada caracter al código binario que le corresponde en el árbol.
buildCodeMap :: HTree -> CodeMap
buildCodeMap t = buildCodeMap' t []

-- Recorrido recursivo del árbol que genera el código para cada hoja (caracter).
-- En lugar de usar code ++ [Zero/One] (que es costoso, pues recorre toda la primer lista para concatenar y esto lo haria en cada nodo del árbol), se construye el código al revés usando (Zero/One : code), y luego lo damos vuelta (reverse) al llegar a una hoja.
buildCodeMap' :: HTree -> [Bit] -> CodeMap
buildCodeMap' (Leaf c _) code = DM.singleton c (reverse code)
buildCodeMap' (Node l r _) code = 
    let leftMap = buildCodeMap' l (Zero : code)
        rightMap = buildCodeMap' r (One : code)
    in DM.union leftMap rightMap


----------------------------------------
-- Ejercicio 5

-- Esta función codifica un String a una secuencia de bits (Code) usando un CodeMap. Para cada carácter en el String, busca su código correspondiente en el CodeMap y luego concatena todos los códigos en orden.
encode :: CodeMap -> String -> Code
encode _ [] = []
encode cmap (c:cs) = 
    case DM.lookup c cmap of
        Just code -> code ++ (encode cmap cs)
        Nothing -> error "Error: caracter no asociado con un código."


----------------------------------------
-- Ejercicio 6

-- Función que dado un árbol de Huffman y un código que representa a un string codificado, decodifica el string y devuelve los caracteres que lo conforman (es decir, el mensaje original).
decode :: HTree -> Code -> String
decode t [] = []
decode t code =
    let (char, rest) = decodeChar t code
    in char : (decode t rest)
    where
        -- Recorremos el árbol con los bits hasta llegar a una hoja, y devolvemos el carácter y el resto del código.
        decodeChar :: HTree -> Code -> (Char, Code)
        decodeChar (Leaf c _) cs = (c, cs)
        decodeChar (Node l r _) [] = error "Error: código incompleto."
        decodeChar (Node l r _) (c:cs) =
            case c of
                Zero -> decodeChar l cs
                One -> decodeChar r cs


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

-- Construir el árbol de Huffman a partir del map engFM.
htreeEng = buildHTree engFM

-- Construir el diccionario de codificación.
codeMapEng = buildCodeMap htreeEng

-- Strings de prueba.
string1 = "hello world."
string2 = "segmentation fault core dumped"
string3 = "p equals np is the most important problem in computer science"
string4 = "may the force be with you"
string5 = "all we have to decide is what to do with the time that is given us"
string6 = "stairway to heaven, the best song of all time"
string7 = "all you need is love"
string8 = "and in the end, the love you take is equal to the love you make"
string9 = "theres a lady whos sure all that glitters is gold and shes buying a stairway to heaven. when she gets there she knows if the stores are all closed with a word she can get what she came for. ooh ooh and shes buying a stairway to heaven. theres a sign on the wall but she wants to be sure cause you know sometimes words have two meanings. in a tree by the brook theres a songbird who sings sometimes all of our thoughts are misgiven. ooh it makes me wonder ooh makes me wonder"
string10 = "it was a bright cold day in april and the clocks were striking thirteen. winston smith his chin nuzzled into his breast in an effort to escape the vile wind slipped quickly through the glass doors of victory mansions though not quickly enough to prevent a swirl of gritty dust from entering along with him."
string11 = "when mr bilbo baggins of bag end announced that he would shortly be celebrating his eleventyfirst birthday with a party of special magnificence there was much talk and excitement in hobbiton. bilbo was very rich and very peculiar and had been the wonder of the shire for sixty years ever since his remarkable disappearance and unexpected return. the riches he had brought back from his travels had now become a local legend though it was popularly believed that most of it was spent on the maintenance of his many nephews and nieces and on the purchase of useless trinkets. he had also lost the respect of many of his neighbors by his odd behavior and his friendship with the elves. the hobbits of the shire were not fond of adventures or anything that disturbed the peace and quiet of their lives and bilbo baggins had been known to go off without warning for days at a time. his return was usually accompanied by strange tales of trolls dragons and mountains that walked. most of the hobbits believed these tales to be nonsense but they still made them uneasy. on this particular morning bilbo was standing at his front door smoking an enormous pipe and looking out over his garden which sloped down to the river. the sun was shining and the grass was very green. he was thinking about his birthday party and especially about the speech he intended to make which was already beginning to trouble him. he had a feeling that it might require some explanation. the idea of a party had been in his mind for some time but he had not mentioned it to anyone until now. he wondered whether he ought to tell gandalf about it. gandalf was a wizard who had been a friend of his for many years and who had a habit of turning up unexpectedly. bilbo was not sure whether gandalf would approve of the idea of a party or whether he would think it a waste of time. he was also not sure whether he wanted gandalf to come to the party at all. wizards were not popular in the shire and bilbo did not want to cause any more talk than was necessary. he sighed and blew a smoke ring which floated away over the trees. it was going to be a very long day."

-- Esta función recibe un string y lo codifica. Luego, devuelve una dupla (Int, Int), donde el primer entero representa la cantidad de bits del codigo correspondiente al string en codificación Huffman y el segundo la cantidad de bits en una codificación de longitud fija de 5 bits por caracter.
compararCodificacion :: String -> (Int, Int)
compararCodificacion str =
    let encodedString = encode codeMapEng str
        bitsHuff = length encodedString
        bitsFijos = 5 * length str
    in (bitsHuff, bitsFijos)

-- Aplicamos la comparacion a cada string.
res1 = compararCodificacion string1 -- (55,60)
res2 = compararCodificacion string2 -- (131,150)
res3 = compararCodificacion string3 -- (263,305)
res4 = compararCodificacion string4 -- (107,125)
res5 = compararCodificacion string5 -- (266,330)
res6 = compararCodificacion string6 -- (187,225)
res7 = compararCodificacion string7 -- (84,100)
res8 = compararCodificacion string8 -- (269,315)
res9 = compararCodificacion string9 -- (1955,2370)
res10 = compararCodificacion string10 -- (1319,1530)
res11 = compararCodificacion string11 -- (8959,10660)

-- Conclusiones.
-- Los resultados demuestran que la codificación Huffman supera ampliamente a una codificación fija de 5 bits por carácter en términos de compresión. Esto se debe a que adapta la longitud de los códigos según la frecuencia de cada carácter, asignando menos bits a los más comunes y más bits a los menos frecuentes.
-- Si bien en textos cortos o con una distribución uniforme de caracteres la mejora puede no ser tan significativa, en textos largos la ventaja se vuelve mucho más notoria, logrando un mayor ahorro de bits.