-- Escribir un programa interactivo que implemente un juego en el que hay
-- que adivinar un número secreto predefinido. El jugador ingresa por 
-- teclado un número y la computadora le dice si el número ingresado es 
-- menor o mayor que el número secreto o si el jugador adivinó, en cuyo
-- caso el juego termina.
-- Ayuda: para convertir String en Int usar función read :: String -> Int

import System.IO
import Control.Monad (unless)

adivinador :: IO ()
adivinador =
    do  putStrLn "Piense en un número"
        numero <- sgetLine
        putStrLn "Intente adivinarlo:"
        adivina numero

-- Desactiva echo de la consola y lee el número de la consola.
-- error hacer read (sgetLine') porque sgetLine' es IO String, no String.
sgetLine :: IO Int
sgetLine =
    do  hSetEcho stdin False
        numero <- sgetLine' 
        hSetEcho stdin True
        return (read numero)

-- Lee un string que representa un número de varios digitos.
sgetLine' :: IO String
sgetLine' =
    do  x <- getChar
        if x == '\n'
            then do putChar x
                    return []
            else do xs <- sgetLine'
                    return (x:xs)

adivina :: Int -> IO ()
adivina numero =
    do  putStr "> "
        linea <- getLine
        let numSug = read linea
        if numSug == numero
            then putStrLn "Adivinaste!"
            else if numSug < numero
                then do putStrLn "El número a adivinar es mayor."
                        adivina numero
                else do putStrLn "El número a adivinar es menor."
                        adivina numero