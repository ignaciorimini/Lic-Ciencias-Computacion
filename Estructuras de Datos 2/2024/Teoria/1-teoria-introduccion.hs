{-PROGRAMACIÓN FUNCIONAL
La programación funcional es un estilo de programación moderno. 
No usa un modelo de computación basado en máquinas sino en un lenguaje simple y elegante (lambda cálculo).
Su simpleza y versatilidad lo hacen ideal para aprender conceptos de:
+ Programación
+ Lenguajes de programación
+ Computabilidad
+ Semántica
+ Verificación, derivación, testing

La programación funcional es un estilo de programación en el cual el método básico de computar es aplicar funciones a argumentos.
Un lenguaje de programación funcional es un lenguaje que permite y alienta el uso de un estilo funcional.

______________________________
LENGUAJE DE PROGRAMACIÓN HASKELL

VENTAJAS DE HASKELL
+ Programas concisos
+ Sistemas de tipos poderoso
+ Funciones recursivas
+ Fácilidad para probar propiedades de programas
+ Funciones de alto orden
+ Evaluación perezosa
+ Fácilidad para definir DSLs
+ Efectos monádicos

PRELUDIO
Muchas funciones de uso común están definidas en el Preludio (Prelude.hs)
Además de las operaciones aritméticas usuales se definen muchas funciones que operan sobre listas (head, tail, (!!), take, drop, length, sum, product, reverse)

COMPILAR
Usaremos GHC, un compilador (ghc) e intérprete (ghci) de Haskell.
Para compilar un archivo archivo.hs debemos utilizar el comando "ghc archivo.hs" y luego "./archivo" para ejecutarlo.


______________________________
OFFSIDE RULE
La "Offside Rule" es una característica sintáctica en Haskell que permite delimitar bloques de código basándose en la indentación en lugar de utilizar llaves {} como en otros lenguajes de programación.

En Haskell, la "Offside Rule" se utiliza para definir la estructura de los bloques de código, como las definiciones de funciones, condicionales (if, else), bucles (do), etc. La indentación se utiliza para indicar el alcance de estas estructuras.

En una serie de definiciones, cada definición debe empezar en la misma columna.
a = b + c
    where
        b = 1
        c = 2
d = a + 2

miFuncion x =
    if x > 0
    then "positivo"
    else "negativo"


______________________________
OPERADORES INFIJOS
Los operadores infijos son funciones como cualquier otra.
Una función se puede hacer infija con backquotes: 10 'div' 4 = div 10 4

Se pueden definir operadores infijos usando alguno de los símbolos disponibles: a ** b = (a*b) + (a+1) * (b-1)

La asociatividad y precedencia se indica usando <infixr> (asociatividad derecha), <infixl> (asociatividad izquierda), o <infix> (si los paréntesis deben ser obligatorios).
infixr 6 (**)

SECCIONES
Un operador infijo, puede ser escrito en forma prefija usando paréntesis:
(+) 1 2 = 3

También uno de los argumentos puede ser incluído en los paréntesis:
(1+) 2
(2+) 1
-}
