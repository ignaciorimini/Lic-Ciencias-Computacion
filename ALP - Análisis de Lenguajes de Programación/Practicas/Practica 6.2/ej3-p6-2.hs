-- 3) Escribir un programa intercativo que implemente un juego en el 
-- que hay que adivinar un número secreto predefinido.
-- El jugador ingresa por teclado un número y la computadora le dice
-- si el número ingresado es menor o mayor que el número secreto
-- o si el jugador adivinó, en cuyo caso el juego termina.
-- Ayuda: para convertir String en Int usar read :: String -> Int

import System.IO
import Control.Monad (unless)
import Text.Read (readMaybe)

adivinador :: IO ()
adivinador =
    do  putStrLn "Ingrese el número a adivinar."
        numero <- sgetLine
        putStrLn "Intente adivinarlo:"
        adivina numero

sgetLine :: IO Int
sgetLine =
    do  hSetEcho stdin False
        input_str <- sgetLine'
        hSetEcho stdin True
        case readMaybe input_str of
            Just num -> do return num
            Nothing -> 
                do  putStrLn "Entrada inválida. Ingresar solo números."
                    sgetLine
        
sgetLine' :: IO String
sgetLine' =
    do  x <- getChar
        if x == '\n'
            then do putChar x
                    return []
            else do xs <- sgetLine'
                    return (x:xs)

adivina :: Int -> IO ()
adivina num =
    do  putStr "> "
        xs <- getLine
        case readMaybe xs of
            Nothing -> 
                do  putStrLn "Entrada inválida. Ingresar solo números."
                    adivina num
            Just n -> 
                if n == num
                then putStrLn "Adivinaste el número!"
                else if n > num 
                then do putStrLn "El número a adivinar es menor."
                        adivina num
                else do putStrLn "El número a adivinar es mayor."
                        adivina num