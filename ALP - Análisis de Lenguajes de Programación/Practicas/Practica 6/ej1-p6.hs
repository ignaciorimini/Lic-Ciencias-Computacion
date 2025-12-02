-- 1) Escribir y compilar un programa (usando ghc en lugar de ghci)
-- que imprima en pantalla la cadena "Hola mundo!".

main :: IO ()
main = do putStrLn "Hola mundo!"

-- Compilar: ghch <nombre_opcional> ej1-p6.hs
-- Ejecutar: ./<nombre_opcional>