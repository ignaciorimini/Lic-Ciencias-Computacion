module Test where
-- Compilar ghci teoria-video-funciones.hs y ejecutar comandos como <division 4 2> en ese intérprete.
-- :r con el archivo abierto para recargar y no tener que volver a compilar.
-- Un módulo es un archivo que dentro tiene funciones definidas. Se pueden importar módulos también.
-- Prelude es un módulo que tiene todas las funcionalidades básicas (como la librería stdlib de C).
-- Se pueden definir funciones en el Prelude, escribiendo solo la definición de la función. Con :t <nombreFuncion> podremos ver el tipo.
-- También las podemos definir con el tipo: tipo_de_la_funcion; definicion_de_la_funcion.

{-
FUNCIONES
- Una función podria no tomar argumentos, pero todas las funciones devuelven un valor. En dicho caso, sería una función constante.
- Todo en Haskell devuelve un valor.
- No es necesario escribir el prototipo de la función, pero ayuda mucho al sistema de tipos.

FUNCIONES PREFIJAS: funciones que se escriben antes del argumento.
- -(-2)

FUNCIONES INFIJAS: funciones que se escriben entre los argumentos.
- 2 + 2 (+ es una función en realidad).

FUNCIONES POSTFIJAS: funciones que se escriben luego de los argumentos.
- 2 cuadrado.

Observación: la función <div> es prefija, pero usando las backticks `div` podemos volverla infija. Esto funciona siempre para funciones prefijas.
-}


-- EJEMPLO FUNCION DIVISION DE ENTEROS
division :: Int -> Int -> Int
division x y = x `div` y

-- <division> es el nombre de la función.
-- Los dos puntos :: indican que la función <division> tiene el siguiente tipo, (tipo de dato de los argumentos y del retorno).
-- El último valor de la cadena es el valor de retorno (tercer Int).
-- Int -> Int es como (Int, Int) -> Int, es decir, <division> toma dos Int.

-- Debajo del prototipo va la definición de la función.
-- x es el primer argumento, es de tipo Int.
-- y es el segundo argumento, es de tipo Int.
-- Lo que está después de = es el retorno, de tipo Int.


-- EJEMPLO FUNCION QUE ELEVA UN ENTERO AL CUADRADO
cuadrado :: Int -> Int
cuadrado x = x * x


{-
APLICACIÓN DE FUNCIONES - EJECUCIÓN DE FUNCIONES
En matemática, la aplicación de una función a sus argumentos es usualmente denotada encerrando los argumentos entre paréntesis.
En Haskell, la aplicación se denota con espacios.
f(a, b) = f a b

En matemática, la multiplicación se puede denotar escribiendo dos valores juntos.
En Haskell, la multiplicación debe explicitarse con un asterisco
cd = c*d

La aplicación de funciones tiene más prioridad que las otras operaciones.
f a b + c * d -> Primero se evalua f(a, b), luego c*d, finalmente f(a, b) + c*d.

La aplicación asocia a izquierda. Si f toma dos argumentos, entonces
f x g y -> tomaria f(x, g) y esto no tipa.
f x (g y) -> sería f(x, g(y)) y sería correcto.

Para llamar a una función ya definida, con argumentos:
division 10 2 
-}
