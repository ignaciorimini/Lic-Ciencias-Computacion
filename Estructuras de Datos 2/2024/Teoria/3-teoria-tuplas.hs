{-
TUPLAS EN HASKELL

_________________
DEFINICIÓN: Una tupla es una secuencia FINITA de valores de tipos (posiblemente) distintos. A diferencia de las listas, las tuplas tienen un tamaño fijo y pueden contener elementos de tipos diferentes.

_________________
DECLARACIÓN DE TUPLAS: En Haskell las tuplas se declaran utilizando paréntesis () para delimitar los elementos de la tupla, separados por comas. Por ejemplo:
+ miTupla :: (Bool, Bool)
+ miTupla = (True, True)

+ miTupla2 :: (Bool, Char, Char)
+ miTupla2 = (True, 'a', 'b')

En general, (t1, t2, ... tn) es el tipo de una n-tupla cuyas componente i tiene tipo ti.

Los tipos de las tuplas no tienen restricciones:
+ ('a', (True, 'c')) :: (Char, (Bool, Char))
+ ((['a', 'b'], 'a'), 'b') :: (([Char], Char), Char)

_________________
ACCESO A LOS ELEMENTOS
Para acceder a los elementos individuales de una tupla, se utiliza la notación de punto (.) seguida de un número entero que representa la posición del elemento en la tupla.

A su vez, Haskell provee las funciones <fst> y <snd> que devuelven el primer y segundo elemento de una tupla respectivamente:
primerElemento = fst miTupla2
-- primerElemento = True

segundoElemento = snd miTupla
-- segundoElemento = 'a'

_________________
PATRONES DE DESESTRUCTURACIÓN DE TUPLAS
Una técnica común en Haskell es usar patrones de desestructuración para extraer los elementos de una tupla en una función:
sumaTupla :: (Int, Int) -> Int
sumaTupla (x, y) = x + y

Los patrones de tuplas permiten descomponer tuplas en sus componentes individuales para su posterior procesamiento. En esencia, es una forma de estructurar y acceder a los elementos de una tupla de manera individual.

En Haskell, una tupla se puede descomponer utilizando el pattern matching al definir funciones. Esto se hace proporcionando un patrón que coincide con la estructura de la tupla en el lado izquierdo de la definición de la función.

Los patrones de tuplas son útiles cuando se trabaja con funciones que toman tuplas como argumentos, ya que permiten descomponer rápidamente la tupla en sus componentes individuales para su posterior procesamiento. Esto evita la necesidad de acceder a los elementos de la tupla usando funciones como fst y snd.

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y
-}