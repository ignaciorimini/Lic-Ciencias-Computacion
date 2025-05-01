{-
LISTAS EN HASKELL

_________________
DEFINICIÓN: las listas son una estructura de datos que se utilizan para almacenar colecciones de elementos del mismo tipo.

_________________
DECLARACIÓN DE LISTAS: en haskell las listas se declaran utilizando corchetes [] para delimitar los elementos de la lista, separados por comas. Por ejemplo:
+ listaBool :: [Bool]
+ listaBool = [True, True, False, True]

+ listaPalabra :: [Char]
+ listaPalabra = ['h','o','l','a']

En general, [t] es una lista con elementos del tipo t. Donde t puede ser cualquier tipo válido.
+ [['a'], ['b','c'], []] :: [[Char]] (lista de listas de char)

_________________
OBSERVACIONES
No hay restricción con respecto a la longitud de las listas.

Por convención, los argumentos de listas usualmente tienen como sufijo la letra s para indicar que pueden contener múltiples valores.
- ns: lista de números.
- xs: lista de valores arbitrarios.
- xss: lista de lista de caracteres.

_________________
OPERACIONES BÁSICAS

Concatenaciión: se utiliza el operador ++
lista1 ++ lista2

Agregar elemento al principio de la lista (cons): se utiliza el operador :
1 : [2, 3, 4, 5]

Obtener longitud de lista: se utiliza la función length que devuelve la longitud de una lista.
length [1, 2, 3, 4, 5]

Acceder según índice: utilizando la función infija <lista> !! <indice> obtendremos el elemento de índice indicado de la lista pasada como argumento.
[1, 2, 3, 4, 5] !! 2 -> devuelve el tercer elemento (índice 2)

_________________
FUNCIONES DE LISTAS: Haskell proporciona una serie de funciones predefinidas para trabajar con listas.

HEAD: devuelve el primer elmento de una lista
lista = [1, 2, 3, 4, 5]
primerElemento = head lista
-- primerElemento = 1

TAIL: devuelve todos los elementos de una lista excepto el primero.
lista = [1, 2, 3, 4, 5]
restoLista = tail lista
-- restoLista = [2, 3, 4, 5]

LAST: devuelve el último elemento de una lista.
lista = [1, 2, 3, 4, 5]
ultimoElemento = last lista
-- ultimoElemento = 5

INIT: devuelve todos los elementos de una lista excepto el último.
lista = [1, 2, 3, 4, 5]
sinUltimoElemento = init lista
-- sinUltimoElemento = [1, 2, 3, 4]

NULL: comprueba si una lista está vacía.
listaVacia = []
estaVacia = null listaVacia
-- estaVacia = True

REVERSE: invierte una lista.
lista = [1, 2, 3, 4, 5]
listaInvertida = reverse lista
-- listaInvertida = [5, 4, 3, 2, 1]

TAKE: toma los primeros n elementos de una lista.
lista = [1, 2, 3, 4, 5]
primerosTresElementos = take 3 lista
-- primerosTresElementos = [1, 2, 3]

DROP: elimina los primeros n elementos de una lista.
lista = [1, 2, 3, 4, 5]
sinPrimerosTresElementos = drop 3 lista
-- sinPrimerosTresElementos = [4, 5]

ELEM: comprueba si un elemento está presente en una lista.
lista = [1, 2, 3, 4, 5]
estaElTres = elem 3 lista
-- estaElTres = True

FILTER: filtra los elementos de una lista según un predicado.
lista = [1, 2, 3, 4, 5]
pares = filter even lista
-- pares = [2, 4]

MAP: aplica una función a cada elemento de una lista.
lista = [1, 2, 3, 4, 5]
cuadrados = map (\x -> x^2) lista
-- cuadrados = [1, 4, 9, 16, 25]

FOLDR/FOLD: realiza un plegado sobre una lista, combinando los elementos usando una función. Toma una función (suma), un valor inicial (0) y una lista. Aplica la función acumulativa a cada elemento de la lista, partiendo desde el lado derecho (r en foldr). La suma acumulada se inicia desde 0.
lista = [1, 2, 3, 4, 5]
suma = foldr (+) 0 lista
-- suma = 15

_________________
LISTAS POR COMPRENSIÓN: Haskell permite construir listas de forma consisa mediante la especificación de reglas de generación y filtros, es decir, generar listas por comprensión.

El siguiente ejemplo genera una lista de los cuadrados de los números del 1 al 10:
cuadrados = [x^2 | x <- [1..10]]

La expresión x <- [1..10] es un generador, ya que dice como se generan los valores de x. Una lista por comprensión puede tener varios generadores, separados por coma.
[(x, y) | x <- [1,2,3], y <- [4,5]]
-- [(1,4), (1,5), (2,4), (2,5), (3,4), (3,5)]

¿Qué pasa cuando cambiamos el orden de los generadores? Un generador puede depender de un generador anterior, a estos los llamamos GENERADORES DEPENDIENTES.
[(x,y) | x <- [1..3], y <- [x..3]]
-- [(1, 1), (1,2), (1,3), (2,2), (2,3), (3,3)]


_________________
PATRONES DE LISTAS
Toda lista (no vacía) se contruye usando el operador (:) llamado cons, que agrega un elemento al principio de la lista.
- [1, 2, 3, 4] -> 1 : (2 : (3 : (4 : [])))

Por lo tanto, se pueden definir funciones usando el patrón (x:xs)
head :: [a] -> a
head (x:_) = x

tail :: [a] -> a
tail (_:xs) = xs

El patrón x:xs solo matchea el caso de listas no vacías. Por lo tanto, ejecutar la función head con una lista vacía arrojaría error.


_________________
FUNCION ZIP
La función ZIP toma dos listas y las combina en una sola lista de pares. Cada par contiene un elemento de la primera lista y un elemento correspondiente de la segunda lista. Zip detiene la combinación cuando una de las listas se agota.

zip :: [a] -> [b] -> [(a,b)]
-- zip ['a', 'b', 'c'][1,2,3,4] -> [('a',1), ('b',2),('c',3)]

-}