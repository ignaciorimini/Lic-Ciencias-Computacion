factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    putStrLn "Ingrese un número para calcular su factorial:"
    num <- readLn
    putStrLn ("El factorial de " ++ show num ++ " es " ++ show (factorial num))
