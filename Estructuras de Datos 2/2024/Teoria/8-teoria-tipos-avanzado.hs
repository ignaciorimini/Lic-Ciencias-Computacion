{-
TIPOS EN HASKELL
- Sistemas de tipos expresivo: Haskell permite definir tipos de datos de manera flexible y expresiva.
- Tipado estético: todos los valores y expresiones tienen un tipo conocido en tiempo de compilación. Esto significa que el tipo de cada expresión se determina antes de que se ejecute el programa, lo que ayuda a detectar errores de tipo en etapas tempranas.
- Inferencia de tipos: el compilador puede inferir automáticamente muchos tipos basados en el contexto del programa. Sin embargo, dar el tipo de las funciones es ventajoso.


----------------------
SINÓNIMOS DE TIPOS
En Haskell se puede definir un nuevo nombre para un tipo existente usando una declaración type. Por ejemplo, luego de la siguiente instrucción String pasaría a ser un sinónimo del tipo [Char].
type String = [Char]

Los sinónimos de tipo hacen que ciertas declaraciones de tipos sean más fáciles de leer.

type Pos = (Int, Int)

origen :: Pos
origen = (0, 0)

izq :: Pos -> Pos
izq (x,y) = (x-1,y)

----
Los sinónimos de tipo pueden tener parámetros.
type Par a = (a, a)
copiar :: a -> Par a
copiar x = (x, x)

Los sinónimos de tipo pueden anidarse.
type Punto = (Int, Int)
type Trans = Punto -> Punto

Pero no pueden ser recursivos.
type Tree = (Int, [Tree])


----------------------
DECLARACIONES DATA
Los data declaran un nuevo tipo cuyos valores se especifican en la declaración. El siguiente ejemplo declara un nuevo tipo llamado Bool con dos nuevos valores: False y True.
+ data Bool = False | True
- True y False son los constructores del tipo Bool
- Los nombres de los constructores deben empezar con mayúsculas.
- Dos constructores diferentes siempre construyen diferentes valores del tipo.

+ data Color = Rojo | Verde | Azul

----
Los constructores pueden tener argumentos. En el siguiente ejemplo, Arbol a es el nombre del nuevo tipo de datos, que toma un tipo a como parámetro. Hay dos constructores de datos: Hoja, que toma un valor de tipo a como argumento, y Nodo, que toma tres arumentos de tipo a y dos subárboles de tipo Arbol a.
+ data Arbol a = Hoja a | Nodo (Arbol a) a (Arbol a)

----
Los valores de un nuevo tipo se usan igual que los predefinidos.
+ data Respuesta = Si | No | Desconocida

respuestas :: [Respuesta]
respuestas = [Si, No, Desconocida]

invertir :: Respuesta -> Respuesta
invertir Si = No
invertir No = Si
invertir Desconocida = Desconocida

----
Ejemplo en que los constructores tienen parámetros.
-- Constructores que representan figuras geométricas: un círculo con radio Float y un rectángulo con dos lados Float.
+ data Shape = Circle Float | Rect Float Float

-- Función que toma un valor Float y devuelve un cuadrado, es decir, un rectángulo con misma longitud de lado.
square :: Float -> Shape
square n = Rect n n

-- Función que calcula el área de una figura geométrica, usando pattern matching. Si es un círculo una fórmula, si es un rectángulo otra.
area :: Shape -> Float
area (Circle r) = π * r^2
area (Rect x y) = x * y

-- Los constructores son funciones:
> :t Circle | Circle :: Float -> Shape
> :t Rect | Rect :: Float -> Float -> Shape


----------------------
RECORDS - REGISTROS
Los registros/records son una extensión de Haskell que permite definir tipos de datos con campos etiquetados. Esto facilita la manipulación de datos estructurados al proporcionar nombres descriptivos para cada campo dentro de un registro. Sintaxis:
data NombreTipo = Constructor {campo1 :: Tipo1, campo2 :: Tipo2, ...}

+ data Alumno = A String String Int String deriving Show

juan :: Alumno
juan = A "Juan" "Perez" 21 "jperez999@gmail.com"

Luego, se puede acceder a los campos del "objeto"/"estructura" definiendo funciones:
nombre :: Alumno -> String
nombre (A n _ _ _) = n

apellido :: Alumno -> String
apellido (A _ a _ _) = a

edad :: Alumno -> Int
edad (A _ _ e _) = e

email :: Alumno -> String
email (A _ _ _ m) = m

----
Haskell provee sintaxis para aliviarnos el trabajo:
+ data Alumno = A {nombre :: String, apellido :: String, edad :: Int, email :: String} deriving Show

Con esta sintaxis no tenemos que definir las proyecciones por separado. Además Cambia la instancia de Show y no tenemos que acordarnos del orden de los campos:

juan = A {apellido = "Perez", nombre = "Juan", email = "jperez999@gmail.com", edad = 21}

Para acceder a los campos usamos la notación de punto:
nombreDeJuan :: String
nombreDeJuan = nombre juan


----------------------
CONSTRUCTOR DE TIPOS MAYBE
Maybe es un tipo de datos algebraico en Haskell que se utiliza para representar valores que pueden estar o no presentes. Es útil para manejar casos en los que una función podría devolver un valor válido o un valor "nulo" (ausencia de un valor).

Definición de Maybe:
data Maybe a = Nothing | Just a
- "Maybe a" es un tipo de datos paramétrico que indica que puede contener un valor de tipo a, o puede estar vacío.
- "Nothing" es un constructor de datos que representa la ausencia de un valor. No lleva ningún argumento.
- "Just a" es un constructor de datos que representa la presencia de un valor. Lleva un argumento de tipo a, que es el valor que está presente.

Kind de Maybe:
Maybe :: * -> *

Maybe suele usarse para señalar una condición de error, o hacer total una función parcial. El siguiente ejemplo muestra como se puede usar Maybe para manejar la posibilidad de que una función devuelva un valor nulo.
- safehead toma una lista de elementos de tipo a. Si la lista está vacía, devuelve "Nothing" para indicar que no hay ningún elemento en la lista. Si la lista tiene al menos un elemento, devuelve Just x, donde x es el primer elemento de la lista.
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Ejemplo de función que recibe una clave y una lista "clave-valor" y devuelve el valor asociado a la clave pasada como argumento si la encuentra en la lista.
lookup :: Eq c => c -> [(c, val)] -> Maybe val
lookup _ [] = Nothing
lookup k ((c, v):xs) 
    | k == c    = Just v
    | otherwise = lookup k xs


----------------------
CONSTRUCTOR DE TIPOS EITHER
Either describe un tipo que puede tener elementos de dos tipos. 
En sus elementos está claro de qué tipo es el elemento almacenado.

Definición de Either:
data Either a b = Left a | Right b
- Either a b: tipo de datos paramétricos que indica que puede contener un valor de tipo a o un valor de tipo b.
- Left a: constructor de datos que representa un resultado fallido. Lleva un argumento de tipo a, que generalmente se usa para almacenar información sobre el error.
- Right b: es un constructor de datos que representa un resultado exitoso. Lleva un argumento de tipo b, que es el valor exitoso que se ha producido.

Kind de Either:
Either :: * -> * -> *

Kind se utiliza para representar resultados que pueden ser uno de dos posibles valores diferentes. Es útil para manejar casos en los que una función puede devolver un resultado exitoso o un error.

safehead :: [a] -> Either String a
safehead [] = Left "head de lista vacía!"
safehead xs = Right (head xs)

divide :: Float -> Float -> Either String Float
divide _ 0 = Left "No se puede dividir por cero"
divide x y = Right (x/y)


----------------------
TIPOS RECURSIVOS
Los tipos recursivos son una característica de Haskell que permite definir tipos de datos que se refieren a sí mismos de forma recursiva. Esto permite crear estructuras de datos complejas y recursivas, como árboles, listas y otras estructuras anidadas.

data List a = Nil | Cons a (List a)
- Constructor Nil representa una lista vacía.
- Constructor Cons representa un nodo en la lista que contiene un valor de tipo a y una referencia al resto de la lista (List a)
- List es una lista enlazada.

----
Tipo de datos que representa números naturales. Su definición es recursiva y tiene el constructor Zero que representa al cero natural, y Succ que representa al sucesor de otro número natural. Toma un argumento de tipo Nat, que es el numero natural del cual es sucesor.
data Nat = Zero | Succ Nat

add :: Nat -> Nat
add n Zero = n
add n (Succ m) = Succ (add n m)


----------------------
EXPRESIONES CASE
Case es una expresión que se utiliza para hacer coincidencia de patrones en el valor de una expresión y ejecutar diferentes acciones basadas en el patrón que coincida. Es una forma de control de flujo que permite manejar diferentes casos de forma explícita y elegante.

Sintaxis:
case expresion of
    patron1 -> expresion1
    patron2 -> expresion2
- expresion: es la expresion cuyo valor queremos analizar.
- patron1, patron2..: son patrones que se comparan con el valor de expresion.
- expresion1, expresion2..: son las expresiones que se evalúan si e valor coincide con el patrón correspondiente.

Los patrones de los diferentes casos son intentados en orden. Se usa identación para marcar un bloque de casos.

esCero :: Nat -> Bool
esCero n = case n of
    Zero    -> True
    _       -> False


----------------------
ÁRBOLES
Para representar diferentes tipos de árboles, podemos definir un tipo de datos recursivo utilizando la declaración data.
- data T1 a = Tip a | Bin (T1 a) (T1 a)
- data T2 b = Empty | Branch (T2 b) b (T2 b)
- data T3 a b = Leaf a | Node (T3 a b) b (T3 a b)
- data T4 a = E | N2 a (T4 a) (T4 a)
                | N3 a (T4 a) (T4 a) (T4 a)

----
Programando con árboles.
- data T1 a = Tip a | Bin (T1 a) (T1 a)

size :: T1 a -> Int
size (Tip _) = 1
size (Bin t1 t2) = size t1 + size t2

depth :: T1 a -> Int
depth (Tip _) = 0
depth (Bin t1 t2) = 1 + (depth t1) 'max' (depth t2)

Medir un árbol de tamaño n es O(n). Se puede hacer en O(1) se si lleva la medida en el árbol.

----
Programando con árboles.

type Weight = Int
data T a = Tip Weight a | Bin Weight (T a) (T a)

-- Función que obtiene la medida es de O(1):
weight :: T a -> Weight
weight (Tip w _)    = w
weight (Bin w _ _)  = w

-- Queremos preservar la siguiente invariante:
weight (Bin w t1 t2) = weight t1 + weight t2

-- Definimos un constructor "inteligente":
bin :: T a -> T a -> T a
bin t1 t2 = let w = weight t1 + weight t2
            in Bin w t1 t2


----------------------
ÁRBOLES DE HUFFMAN
La codificación de Huffman permite comprimir una secuencia de símbolos. Asigna a cada símbolo una secuencia de bits según su probabilidad: a los símbolos más frecuentes les asigna una secuencia corta; a los menos frecuentes, una secuencia más larga.

La tabla de códigos se codifica en un árbol binario con información en las hojas.

La secuencia test nos da origen al árbol
        NULL
    NULL    t
E       s   
Y los códigos correspondientes son: t= 1, e= 00, s = 01

Representación del árbol con el tipo T definido anteriormente.
type Weight = Int
data T a = Tip Weight a | Bin Weight (T a) (T a) 

Para decodificar seguimos un camino hasta una hoja.
data Step = I | D deriving Show
type Camino = [Step]

trace1 :: T a -> Camino -> a
trace1 (Tip _ x) []             = x
trace1 (Bin _ t1 t2) (I : cs)   = trace1 t1 cs
trace1 (Bin _ t1 t2) (D : cs)   = trace1 t2 cs

-}