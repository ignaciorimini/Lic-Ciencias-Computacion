-- Recibe un argumento numérico 1 o 2, que indica que jugador
-- arranca jugando en la partida.
iniciarNim :: Int -> IO ()
iniciarNim n =
    do  putStrLn "Elija una fila y la cantidad de fichas a sacar."
        tableroInicial
        if n > 2 || n < 0 
            then putStrLn ("Jugador 1 juega.")
            else putStrLn ("Jugador " ++ show n ++ " juega")
        putStrLn "Elija una fila:"
        fila <- getChar
        putStrLn "Elija cantidad a sacar:"
        cantidad <- getChar
        tablero (read fila, read cantidad)
        
tableroInicial :: (Int, Int) -> IO ()
tableroInicial (fila, cant) =
    do  putStrLn "1: * * * * *"
        putStrLn "2: * * * *"
        putStrLn "3: * * *"
        putStrLn "4: * *"
        putStrLn "5: *"