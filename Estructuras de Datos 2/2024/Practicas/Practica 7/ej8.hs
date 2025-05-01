-- a) Si el promedio de los resultados de los exámenes es mayor o igual a 70, el estudiante ingresará a la universidad.
-- b) Si el promedio es mayor a 50 y menor a 70, quedará en lista de espera.
-- c) Si el promedio es menor o igual a 50, el alumno no podrá ingresar a la universidad.
-- Se desea saber cuantos estudiantes aprobaron el ingreso, cuantos quedaron en lista de espera y cuantos no podrán ingresar a la universidad, junto con las notas máxima de cada uno de los 3 casos.

-- Dada una secuencia con los nombres de los estudiantes y las notas de los exámenes, calcula los datos necesarios (cantidad de alumnos, nota máxima) de los exámenes de ingreso. Definirla en términos de mapCollectReduce.
-- Ejemplo: (5 aprobados, 10 nota máxima de los aprobados)

-- 1. <("Alice", <80, 90, 70>), ("Bob", <60, 50, 55>), ("Nacho", <80, 80, 80>))
-- 2. <("Aprobado", <80, 90, 70>), ("Espera", <60, 50, 55>), ("Aprobado", <80, 80, 80>)>
-- 3. <("Aprobado", <<80, 90, 70>, <80, 80, 80>>), ("Espera", <<60, 50, 55>>)>
-- 4. <(2, <90, 80>), (1, <60>)>
-- 5. <(2, 90), (1, 60)>

datosIngreso :: Seq (String, Seq Int) -> Seq (Int, Int)
datosIngreso s
    | isEmptyS s = emptyS
    | otherwise =
        let -- Paso 1: Calcular el promedio y clasificar cada estudiante
            clasificados = mapS (\(nombre, notas) -> (clasificar notas, notas)) s

            -- Paso 2: Agrupar los estudiantes por clasificación
            agrupados = collectS clasificados

            -- Paso 3: Contar estudiantes y encontrar la nota máxima para cada categoría
            resultados = mapS (\(categoria, conjNotas) -> (lengthS conjNotas, maxNota conjNotas)) agrupados
        in resultados
    where
        -- Función para calcular el promedio de una secuencia de notas
        promedio :: Seq Int -> Float
        promedio notas = 
            let suma = fromIntegral (reduceS (+) 0 notas)
                cantidad = fromIntegral (lengthS notas)
            in suma / cantidad

        -- Función para clasificar a un estudiante según su promedio
        clasificar :: Seq Int -> String
        clasificar notas = 
            let promedioNotas = promedio notas
            in if promedioNotas >= 70
                then "Aprobado"
                else if promedioNotas > 50 && promedioNotas < 70
                    then "Espera"
                    else "Desaprobado"

        -- Función para encontrar la nota máxima en una secuencia de notas
        maxNota :: Seq (Seq Int) -> Int
        maxNota conjNotas = 
            let reducirNotas = reduceS (\seq -> reduceS max 0 seq) empty conjNotas
            in reduceS max 0 reducirNotas

        
