module Test where

-- Indicar si cada una de las siguientes expresiones está bien o no formada. En caso de que lo esté, determinar el valor que denota, en caso contrario especificar si el error es sintáctico o de tipos:

-- a) if true then false else true where false = True; true = False
-- if true = False -> else true = False -> False
-- La expresión es correcta y devuelve False. 
funcion :: Bool
funcion = if true then false else true
    where
        false = True
        true = False


-- b) if if then then else else
-- Error sintáctico. Luego del if va una condición que evalúa un Booleano y luego del then y del else va una expresión que tenga mismo tipo.


-- c) False == (5 >= 4)
-- Expresión correcta. Pregunta si False == (5 >= 4) -> False == True -> False.


-- d) 1 < 2 < 3
-- Error sintáctico. Haskell no permite comparaciones encadenadas de esta manera. El operador < recibe solo 2 argumentos.


-- e) 1 + if ('a' < 'z') then -1 else 0
-- Expresión correcta. 1 + -1 = 0


-- f) if fst p then fst p else snd p where p = (True, 2)
-- Error de tipo. Las ramas then y else deben devolver un dato del mismo tipo. en este caso la rama then devuelve "fst p" = True, y la rama else devuelve "snd p" = 2.


-- g) if fst p then fst p else snd p where p = (True, False)
-- Expresión correcta. If fst p = True -> then fst p -> True