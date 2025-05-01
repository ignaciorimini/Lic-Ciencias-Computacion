{-

TIPOS EN HASKELL
Un tipo es un nombre para una colección de valores.
+ Bool contiene los valores True y False.
+ Escribimos True::Bool y False::Bool.

En general, si una expresión e tiene tipo t escribimos e::t.

En Haskell, toda expresión válida tiene un tipo. El tipo de cada expresión es calculado previo a su evaluación mediante la inferencia de tipos.

Si no es posible encontrar un tipo (por ejemplo (True+4)) el compilador/intérprete protestará con un error de tipo.

Alguno de los tipos básicos de Haskell son:
+ Bool: booleanos
+ Char: caracteres
+ Int: enteros de precisión fija
+ Integer: enteros de precisión arbitraria
+ Float: números de punto flotante de precisión simple


______________________________
NOMBRES DE LOS TIPOS
A excepción de listas, tuplas y funciones, los nombres de los tipos concretos comienzan con mayúsculas.

El espacio de nombre de los tipos está completamente separado del espacio de nombres de las expresiones.


______________________________
CLASES DE TIPO
Las clases de tipo son un mecanismo que permite definir interfaces de tipos y proporcionar implementaciones para esas interfaces.
Las clases de tipo se utilizan para agregar restricciones sobre los tipos de datos ue pueden ser utilizados con ciertas funciones u operadores.

Una clase de tipo define un conjunto de funciones o métodos que deben ser implementados para que un tipo de datos sea considerado miembro de esa clase. Esas funciones se conocen como "métodos de clase/tipo".

Cuando se declara una instancia de una clase de tipo para un tipo de datos específico, se proporcionan implementaciones concretas para los métodos de clase definidos en la clase de tipo.

________
CLASE EQ
Por ejemplo, la clase de tipo "eq" define la interfaz para la igualdad entre valores. A esta clase van a pertenecer los tipos de datos que pueden ser comparables (las funciones no pueden ser comparables entonces no pertenecen).

Para que un tipo de datos sea miembro de la clase "eq", debe proporcionar una implementación de la función "==" (igualdad) y "/=" (desigualdad).

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

_________
CLASE ORD
Tenemos otra clase, Ord, que son los tipos que además de ser instancias de Eq poseen un orden total:
class Ord a where
    (<), (<=), (>), (>=) :: a -> a -> Bool
    min, max :: a -> a -> a

_________
CLASE SHOW
Son los tipos de datos cuyos valores pueden ser convertidos en una cadena de caracteres:
class Show a where
    show :: a -> a -> String

_________
CLASE READ
Read es la clase dual. Son los tipos de datos que se pueden obtener de una cadena de caracteres:
class Read a where
    read :: a -> String -> a

+ not(read "False") :: Bool
+ read "3" :: Int

_________
CLASE NUM
Son los tipos de datos que son numéricos (Int, Integer, Float, etc)
Sus instancias deben implementar las funciones:
class Num a where
    (+), (-), (*) :: a -> a -> a
    negate, abs, signum :: a -> a

_________
CLASE INTEGRAL
Son los tipos de datos que son Num y además implementan:
class Integral a where
    div, mod :: a -> a -> a

Notar que Num no incluye la división, sino que está función se incluye en la clase Integral.

_________
CLASE FRACTIONAL
Son los tipos de datos que son Num y además implementan:
class Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
-}