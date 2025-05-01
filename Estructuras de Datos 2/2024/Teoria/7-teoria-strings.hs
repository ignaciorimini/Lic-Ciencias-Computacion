{-
CADENAS/STRINGS
Un String es una lista de caracteres.
- "Hola" :: String
- "Hola" = ['H', 'o', 'l', 'a']

Todas las funciones sobre listas son aplicables a String, y las listas por comprensión pueden ser aplicadas a Strings.
cantminusc :: String -> Int
cantminusc xs = length[x | x <- xs, isLower x]

-}