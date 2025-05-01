{-
FUNCIONES EN HASKELL

_________________
INTRODUCCIÓN: Las funciones son fundamentales en Haskell pues son tratadas como ciudadanos de primera clase, lo que significa que pueden ser pasadas como argumentos a otras funciones, devueltas como resultados de otras funciones, y asignadas a variables.

_________________
ASOCIATIVIDAD Y PRECEDENCIA
En Haskell la aplicación de funciones se denota con un espacio y asocia a la izquierda: 
+ f x = f(x)
+ f x y = f(x, y)
+ f (g x) = f(g(x))
+ f x (g y) = f(x, g(y))
+ f x * g y = f(x) g(y)

La aplicación de función tiene mayor precedencia que cualquier otro operador: f x + y = f(x) + y

_________________
DECLARACIÓN DE FUNCIONES
En Haskell las funciones se definen utilizando la palabra clave "f :: a -> b"
donde f es el nombre de la función, a es el tipo de los argumentos de entrada y b el tipo del resultado de la función. 

Por ejemplo, la siguiente función toma un entero y devuelve el doble de ese entero:
doble :: Int -> Int
doble x = x * 2


Una función mapea valores de un tipo en valores de otro tipo.
+ not :: Bool -> Bool
+ isDigit :: Char -> Bool

En general, un tipo de función t1 -> t2 mapea valores de t1 en valores de t2.

Se pueden escribir funciones con múltiples argumentos o resultados usando tuplas y listas.
+ add :: (Int, Int) -> Int | add(x,y) = x+y
+ deceroa :: Int -> [Int]  | deceroa n = [0..n]

_________________
LLAMADA A FUNCIONES
Las funciones en Haskell se llaman simplemente escribiendo el nombre de la función seguido de los argumentos entre espacios. Por ejemplo:
resultado = doble 5
-- resultado = 10

_________________
FUNCIONES ANÓNIMAS (LAMBDA)
En Haskell también se pueden definir funciones anónimas utilizando la sintaxis de lambda. Por ejemplo, una función que suma dos números podría ser definida así:
suma :: Int -> Int -> Int
suma = \x y -> x + y

La forma de función lambda es \lambda x -> x + x
En Haskell escribimos \ en lugar de la letra griega lambda.

La sintaxis básica de una función lambda es:
\x -> expresion
- \ es la letra lambda, que indica que se está definiendo una función anónima.
- x es el argumento de la función.
- expresion es el cuerpo de la función.

Ejemplo función que toma tres argumentos y devuelve la multiplicación entre ellos.
\x y z -> x * y * z

Usando lambdas podemos explicitar que las funciones son simplemente valores:
add x y = x + y
add = \x -> (\y -> x + y)


_________________
FUNCIONES DE ORDEN SUPERIOR
Haskell admite funciones de orden superior, lo que significa que podemos pasar funciones como argumentos a otras funciones y devolver funciones como resultados. 

En el siguiente ejemplo, <aplicar> toma una función f que toma un argumento de tipo a y devuelve un resultado de tipo b, junto con un valor de tipo a, y aplica la función f al valor x:
aplicar :: (a -> b) -> a -> b
aplicar f x = f x

_________________
RECURSIÓN
La recursión es la técnica para reemplazar a los bucles for en Haskell. Haskell admite la recursión tanto en funciones como en estructuras de datos.

El siguiente ejemplo, se calcula el factorial de un número utilizando una función recursiva:
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

_________________
PATRONES DE COINCIDENCIA
Patrones de coincidencia son una característica de Haskell que permite definir diferentes casos de comportamiento para una función dependiendo los valores de los argumentos. Por ejemplo:

esCero :: Int -> Bool
esCero 0 = True
esCero _ = False

_________________
CURRYING
El currying es una técnica en la que una función que toma múltiples argumentos se convierte en una secuencia de funciones que toman un solo argumento. En Haskell, todas las funciones son curriadas de forma predeterminada, lo que significa que podemos aplicar parcialmente una función para obtener una nueva función que espera el resto de los argumentos.

La función suma toma dos argumentos enteros y devuelve su suma. Sin embargo, en Haskell, también podemos entender esta función como una función que toma un solo argumento y devuelve otra función que toma el segundo argumento. Entonces podemos reescribir la definicion de suma:

suma :: Int -> Int -> Int
suma x y = x + y

suma :: Int -> (Int -> Int)
suma x = \y -> x + y

Ahora, si aplicamos parcialmente la función suma a un solo argumento, obtendremos una nueva función que espera el segundo argumento:
sumaDos :: Int -> Int
sumaDos = suma 2

resultado = sumaDos 3
-- resultado= 5

En la notación Int -> Int -> Int, cada -> representa una función que toma un solo argumento y devuelve otro valor. Por lo tanto, podemos leer esto como "una función que toma un entero y devuelve otra función que toma un entero y devuelve un entero".

______________________________
CURRIFICACION Y APLICACIÓN PARCIAL
Otra manera de tomar multiples argumentos es devolver una función como resultado.
add' :: Int -> (Int -> Int) | add' x y = x + y

A diferencia de add, add' toma los argumentos de a uno por vez. Se dice que add' está currificada.
La ventaja de la versión currificada es que permite la aplicación parcial.
suma3 :: Int -> Int
suma3 = add' 3

Si queremos expresar una función que tome 3 argumentos devolvemos una función que devuelve una función:
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

Para evitar escribir muchos paréntesis, por convención el constructor de tipos -> asocia a la derecha: mult :: Int -> Int -> Int -> Int

En Haskell todas las funciones están currificadas (salvo algunos casos particulares).

______________________________
POLIMORFISMO (Polimorfismo paramétrico)
El polimorfismo en Haskell se refiere a la capacidad de una función de trabajar con múltiples tipos de datos de manera genérica.

Una función es polimórfica si su tipo contiene variables de tipo. Es decir, es una función que puede trabajar con múltiples tipos de datos. Puede tomar argumentos de diferentes tipos y realizar las mismas operaciones independientemente del tipo de datos que reciba. Por ejemplo:
lenght :: [a] -> Int

Para cualquier lista de cualquier tipo a, la función length que calcula la cantidad de elementos de una lista es la misma.

Otro ejemplo de función polimórfica que intercambia los elementos de una tupla:
intercambiar :: (a, b) -> (b, a)
intercambiar (x, y) = (y, x)

En este caso, "a" y "b" son variables de tipo, lo que significa que "intercambiar" puede trabajar con tuplas de cualquier tipo de elementos. Por lo tanto
-- intercambiar (1, "Hola") producirá ("Hola", 1)
-- intercambiar ("Mundo", True) producirá (True, "Mundo")


______________________________
POLIMORFISMO DE SOBRECARGA DE FUNCIONES (Polimorfismo ad-hoc)
La sobrecarga de funciones/operadores o polimorfismo ad-hoc es una característica que permite definir múltiples versiones de una función u operador, con el mismo nombre pero de distintos tipos de argumentos.

Por ejemplo la función suma (+) permite sumar Ints, Floats y otros tipos numéricos.

(+) :: Num a => a -> a -> a
(+) está definido para cualquier cualquier tipo que sea una instancia de la clase Num.

A diferencia del polimorfismo paramétrico, hay una definición distinta de (+) para cada instancia.

En Haskell, los números y operaciones aritméticas están sobrecargadas. Cual es el tipo de 3 + 2?

Ejemplo de polimorfismo de sobrecarga:
enteroToString :: Int -> String
enteroToString x = show x

caracterToString :: Char -> String
caracterToString c = show c

______________________________
VALORES LOCALES EN FUNCIONES
La claúsula where se utiliza para definir nombres locales dentro de una función. Estos nombres locales solo son visibles dentro de la función en la que se definen.

Es útil para evitar la repetición de cálculos y para mejorar la legibilidad del código al encapsular detalles de implementación dentro de la función donde son relevantes.

areaCirculo :: Double -> Double
areaCirculo radio = pi * radioCuadrado
    where
        radioCuadrado = radio * radio

"Let" y "In" son palabras clave en Haskell que se utilizan para definir nombres locales dentro de una expresión. A menudo se utilizan para definir valores locales dentro de una función de manera similar a "where".
Let se utiliza para introducir las definiciones locales. Se pueden definir uno o más nombres locales, cada uno seguido por una expresión que le asigna un valor.
In se utiliza para separar las definiciones locales de la expresión principal. Indica dónde comienza la expresión principal que utiliza los nombres locales previamente definidos.

suma :: Int -> Int -> Int
suma x y =
    let resultado = x + y
    in resultado

_________________
OBSERVACIONES
Las funciones y sus argumentos deben empezar con minúscula, y pueden ser seguidos por cero o más letras (mayúsculas o minúsculas), dígitos, guiones bajos, y apóstrofes.

Las palabras reservadas son: case - class - data - default - deriving - do - else - if - import - in - infix - infixl - infxr - instance - let - module - newtype - of - then - type - where

Los comentarios se escriben con --. {-Bloque de comentarios-}.
-}