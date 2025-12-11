-- 5) Un programa pasa todos los caracteres de un archivo de entrada
-- a mayúsculas y los guarda en un archivo de salida. 
-- Hacer un programa compilado que lo implemente tomando dos argumentos
-- en la línea de comandos: nombre del archivo de entrada y nombre
-- del archivo de salida.

import Data.Char (toUpper)

archivoAMayusculas :: IO ()
archivoAMayusculas =
    do  putStrLn "Ingresa archivo de entrada: "
        putStr "> "
        inputFile <- getLine
        putStrLn "Ingresa archivo de salida: "
        putStr "> "
        outputFile <- getLine

        contentInputFile <- readFile inputFile
        let contenidoMayusculas = map toUpper contentInputFile
        writeFile outputFile contenidoMayusculas

        putStrLn "\n--- Proceso Completado ---"
        putStrLn $ "Contenido de " ++ inputFile ++ " transformado y guardado en mayúsculas en archivo " ++ outputFile 