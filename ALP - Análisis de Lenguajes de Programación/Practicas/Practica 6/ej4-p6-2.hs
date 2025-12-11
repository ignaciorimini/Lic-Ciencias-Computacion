import System.IO (hSetEcho, stdin, stdout, hFlush)
import Text.Read (readMaybe)
import Control.Monad (unless)

--------------------------------------------
-- Tipos y constantes.

-- Un Tablero es una lista de enteros, donde cada entero es la cantidad
-- de fichas en esa fila (posición dentro de la lista).
type Tablero = [Int]

-- Estado inicial del juego: 5, 4, 3, 2, 1 fichas por fila.
tableroInicial :: Tablero
tableroInicial = [5,4,3,2,1]

--------------------------------------------
-- Entrada segura.

-- Intenta leer una String a un tipo 'a'. Usa readMaybe.
safeRead :: Read a => String -> Maybe a
safeRead = readMaybe

-- Bucle de lectura de entrada con reintento hasta obtener un valor válido.
-- Muestra el mensaje de prompt y el mensaje de error si falla.
readValidInput :: Read a => String -> String -> IO a
readValidInput prompt errorMsg =
    do  putStr prompt
        hFlush stdout
        inputStr <- getLine
        case safeRead inputStr of
            Just val -> return val
            Nothing -> do   putStrLn errorMsg
                            readValidInput prompt errorMsg

-- Bucle de lectura de entrada sin eco con reintento.
readValidHiddenInput :: Read a => String -> String -> IO a
readValidHiddenInput prompt errorMsg =
    do  putStr prompt
        hFlush stdout

        hSetEcho stdin False
        inputStr <- getLine
        hSetEcho stdin True
        putStrLn ""

        case safeRead inputStr of
            Just val -> return val
            Nothing -> do   putStrLn errorMsg
                            readValidHiddenInput prompt errorMsg

--------------------------------------------
-- Funciones de dibujo y visualización.

-- Dibuja una única fila: número de fila y cantidad de asteriscos.
putCustom :: (Int, Int) -> IO ()
putCustom (fila, cant) =
    do  putStr (show fila ++ ": ")
        sequenceA (replicate cant (putStr "*"))
        putStrLn ""

-- Dibuja el tablero completo.
-- Recibe la lista de fichas y genera la visualización fila por fila.
dibujarTablero :: Tablero -> IO ()
dibujarTablero fichas =
    do  putStrLn "\n--- Tablero Actual ---"
        -- Zip para emparejar (fila, cantidad fichas)
        mapM_ putCustom (zip [1..] fichas)
        putStrLn "----------------------\n"

--------------------------------------------
-- Lógica del juego.

-- Aplica un movimiento al tablero. (Pura, sin IO).
-- Recibe el índice de la fila (0-indexado) y la cantidad a sacar.
aplicarMovimiento :: Tablero -> Int -> Int -> Tablero
aplicarMovimiento tablero filaIndex cantidad =
    -- Utiliza take y drop para reconstruir la lista con la fila modificada.
    let fichasActuales = tablero !! filaIndex
        nuevasFichas = fichasActuales - cantidad
    in take filaIndex tablero ++ [nuevasFichas] ++ drop (filaIndex + 1) tablero

-- Valida si un movimiento es legal. (Pura).
esMovimientoValido :: Tablero -> Int -> Int -> Bool
esMovimientoValido tablero filaIndex cantidad
    | filaIndex < 0 || filaIndex >= length tablero = False
    | cantidad <= 0 = False
    | cantidad > (tablero !! filaIndex) = False
    | otherwise = True

--------------------------------------------
-- Bucle principal del juego.

-- Bucle recursivo del juego. Maneja los turnos, entrada y lógica.
jugarNim :: Tablero -> Int -> IO ()
jugarNim tablero jugador =
    do  if all (== 0) tablero 
            then do -- Condición de victoria.
                    putStrLn $ "JUGADOR " ++ show (siguienteJugador jugador) ++ " GANA!"
                    putStrLn $ "El último movimiento fue del jugador " ++ show jugador ++ "."
            
            else do -- Dibujamos tablero y anunciamos turno.
                    dibujarTablero tablero
                    putStrLn $ "Turno del Jugador " ++ show jugador

                    -- Entrada y validación del movimiento.
                    (fila, cantidad) <- leerMovimiento tablero

                    -- Aplicar movimiento y continuar.
                    let nuevoTablero = aplicarMovimiento tablero (fila - 1) cantidad
                    jugarNim nuevoTablero (siguienteJugador jugador)
                    
-- Pide al usuario el movimiento y valida que sea legal. Bucle de reintento en IO.
leerMovimiento :: Tablero -> IO (Int, Int)
leerMovimiento tablero =
    do  -- Lectura de fila.
        fila <- readValidInput "Fila (1 a 5): " "Entrada inválida. Debe ser un número de fila (1-5)."
        
        -- Lectura de cantidad.
        cantidad <- readValidInput "Cantidad a sacar: " "Entrada inválida. Debe ser un número entero positivo."

        -- Validación.
        let filaIndex = fila - 1
        if esMovimientoValido tablero filaIndex cantidad
            then return (fila, cantidad)
            else do putStrLn "\n--- MOVIMIENTO ILEGAL ---"
                    putStrLn "Debe elegir una fila existente y sacar una cantidad posible."
                    leerMovimiento tablero

-- Función auxiliar para alternar jugadores.
siguienteJugador :: Int -> Int
siguienteJugador 1 = 2
siguienteJugador 2 = 1
siguienteJugador n = 1

--------------------------------------------
-- Función principal de inicio.

-- Recibe el argumento numérico 1 o 2, que indica que jugador arranca jugando.
iniciarNim :: Int -> IO ()
iniciarNim n =
    do  putStrLn "========================================"
        putStrLn "         JUEGO DE NIM (Haskell IO)      "
        putStrLn "========================================"

        -- Normalizar el jugador inicial a 1 o 2.
        let jugadorInicial = if n == 1 || n == 2 then n else 1
        putStrLn $ "El Jugador " ++ show jugadorInicial ++ " inicia la partida."
        jugarNim tableroInicial jugadorInicial