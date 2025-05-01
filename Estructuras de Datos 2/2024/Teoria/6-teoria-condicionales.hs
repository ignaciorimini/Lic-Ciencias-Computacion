{-
______________________________
EXPRESIONES CONDICIONALES
Las expresiones condicionales son una forma de control de flujo que permite ejecutar diferentes bloques de código según una condición específica. En Haskell se realizan a través de if-then-else.

Para que la expresión condicional tenga sentido, ambas ramas de la misma deben tener el mismo tipo. Además, siempre deben tener la rama else para que no haya ambiguedades en caso de anidamiento.

esPositivo :: Int -> Bool
esPositivo x = if x > 0 then True else False

abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else
                if n == 0 then 0 else 1

______________________________
ECUACIONES CON GUARDAS
Una alternativa a los condicionales es el uso de ecuaciones con guardas. En este caso, "|" se utiliza para definir múltiples casos y "otherwise" se utiliza como un patrón de "cualquier otro caso".
Las guardas se evalúan en orden, si una guarda se evalúa como True, se ejecuta le expresión correspondiente. Otherwise = True según el preludio.

esPositivo :: Int -> Bool
esPositivo x
    | x > 0     = True
    | otherwise = False

______________________________
PATTERN MATCHING (o coincidencia de patrones).
El pattern matching es una característica que permite escribir funciones que se comportan de manera diferente según la forma de los datos de entrada. En esencia, el pattern matching permite descomponer estructuras de datos y tomar decisiones basadas en la forma de esas estructuras.

El pattern matching se utiliza principalmente en la definición de funciones para manejar diferentes casos según los valores de entrada. Se puede aplicar a listas, tuplas, tipos algebraicos y otros tipos de datos.

Es como definir una función por partes en matemática.

not :: Bool -> Bool
not False = True
not True = False

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
esFinDeSemana :: DiaSemana -> Bool
esFinDeSemana Sabado = True
esFinDeSemana Domingo = True
esFinDeSemana _ = False

(^) :: Bool -> Bool -> Bool
True ^ True = True
_ ^ _ = False

-}