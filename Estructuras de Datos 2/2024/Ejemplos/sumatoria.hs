sumatoriaLista :: Num a => [a] -> a
sumatoriaLista lista = sum lista

main :: IO ()
main = do
    let numeros = [1, 2, 3, 4, 5]  -- Lista de números
        resultado = sumatoriaLista numeros  -- Calcula la suma de los elementos de la lista
    putStrLn $ "La suma de los elementos de la lista es: " ++ show resultado
