// Recibe una matriz y un entero que puede ser 1 o 2.
// met = 1: convergencia de la matriz a través de Jacobi.
// met = 2: convergencia de la matriz a través de Gauss-Seidel.
function v = condConvergencia(A,met)
    // Creación matriz N.
    if (met == 1) then
        N = diag(diag(A));
    elseif (met == 2) then
        N = tril(A);
    else
        error("condConvergenciaJacobi: codigo mal ingresado, debe ser 1 o 2");
    end
    
    // Chequeamos si la matriz A es cuadrada.
    [n,m] = size(A);
    if n <> m then
        error("condConvergenciaJacobi: la matriz no es cuadrada");
    end
    
    // Chequeamos si la matriz N es singular.
    if prod(diag(N)) == 0 then
        v = 0;
        disp("No se asegura convergencia: la matriz N es singular");
        return;
    end
    
    // Primer condición: Chequeamos si A es diagonal dominante.
    esDiagonalDominante = 1;
    for i=1:n
        elementoDiagonal = abs(A(i,i));
        sumatoria = sum(abs(A(i,1:i-1))) + sum(abs(A(i,i+1:n)));
        if elementoDiagonal <= sumatoria then
            esDiagonalDominante = 0;
            break; 
        end
    end
    
    if esDiagonalDominante == 1 then
        v = 1;
        disp("La matriz converge por ser diagonal dominante");
        return;
    end
    
    // Segunda condición: Chequeamos condición suficiente con norma infinito.
    matriz = eye(n,n) - inv(N)*A;   // Matriz del métod0 iterativo.
    
    norma = norm(matriz, %inf);
    if norma < 1 then
        v = 1;
        disp("Matriz converge por norma: " + string(norma));
        return;
    end
    
    // Tercera condición: Chequeamos condición suf y nec con autovalores.
    autovalores = spec(matriz);
    radioEspectral = max(abs(autovalores));
    if radioEspectral < 1 then
        v = 1;
        disp("Matriz converge por radio espectral: " + string(radioEspectral));
        return;
    end
    
    v = 0;
endfunction

// Matrices del ejercicio.
A = [0 2 4; 1 -1 -3; 1 -2 2];
B = [1 -1 0; -1 2 -1; 0 -1 1.1];

// Convergencia por Jacobi.
v1 = condConvergencia(A,1);
disp(v1);

v2 = condConvergencia(B,1);
disp(v2);

// Convergencia por Gauss-Seidel.
v1 = condConvergencia(A,2);
disp(v1);

v2 = condConvergencia(B,2);
disp(v2);
