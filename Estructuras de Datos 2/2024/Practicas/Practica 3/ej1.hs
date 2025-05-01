module Test where

{-
El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores primarios. Cualquier
otro color se expresa en terminos de los porcentajes de cada uno estos tres colores que es necesario combinar
en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada color de manera biunivoca, por lo que
usualmente se utilizan estos valores como representacion de un color.

Definir un tipo Color en este modelo y una funcion mezclar que permita obtener el promedio componente a
componente entre dos colores.
-}

data Color = RGB Int Int Int deriving Show

crearColor :: Int -> Int -> Int -> Color
crearColor r v a = RGB r v a

mezclar :: Color -> Color -> Color
mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) =
    let r = div (r1 + r2) 2
        g = div (g1 + g2) 2
        b = div (b1 + b2) 2
    in RGB r g b