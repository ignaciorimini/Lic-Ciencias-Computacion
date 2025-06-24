import Seq
import ListSeq

-- Dada una secuencia con los nombres de los estudiantes y las notas de los exámenes, calcula los datos necesarios (cantidad de alumnos, nota máxima) de los exámenes de ingreso.
datosIngreso :: Seq s => s (String, s Int) -> s (Int, Int)
datosIngreso seq =
    let seqParGrupoNotamax = mapS funAux seq
        seqGrupoNotasMax = collectS seqParGrupoNotamax
    in mapS funAux2 seqGrupoNotasMax
    where
        funAux (nombre, seqNotas) =
            let n = lengthS seqNotas
                suma = reduceS (+) 0 seqNotas
                promedio = suma `div` n
                notaMax = maxE compare seqNotas
            in if promedio >= 70
                then (1, notaMax) 
                else if promedio > 50
                then (2, notaMax)
                else (3, notaMax)

        funAux2 (_, seqNotasMax) =
            let n = lengthS seqNotasMax
                notaMaxGrupo = maxE compare seqNotasMax
            in (n, notaMaxGrupo)

----------------------------------------------
-- Ejemplo.
seq 1 = fromList [("Juan", fromList [80, 90]), ("Ana", fromList [60, 65]), ("Luis", fromList [40, 50])]
