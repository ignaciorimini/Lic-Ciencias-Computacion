-- Indicar si cada una de las siguientes expresiones está o no bien formada. En caso de que lo esté determinar el valor que denota, en caso contrario especificar si el error es sintáctico o de tipos.

-- Error de sintaxis por el punto y coma (;) en claúsula
if true then false else true where false = True; true = False

-- Error de sintaxis.
if if then then else else

-- Bien formada: devuelve False.
False == (5 >= 4)

-- Error de tipos: la función (<) recibe dos argumentos de la clase Ord. Al evaluar la primera parte (1 < 2) devuelve un Booleano y lo trata de comparar con 3, donde no son comparables.
1 < 2 < 3

-- Bien formada: devuelve 0.
1 + if ('a' < 'z') then -1 else 0

-- Mal formada: ambas ramas de la sentencia if deben tener el mismo tipo. En este caso la rama "then" tiene tipo Bool y la rama "else" tiene tipo Int.
if fst p then fst p else snd p where p = (True, 2)

-- Bien formada: devuelve True.
if fst p then fst p else snd p where p = (True, False)