// MetodO de eliminación Gaussiana con pivoteo parcial.
// Notar que no se pide la hipótesis de que A tenga elementos distinto de ceros en la diagonal.
function [x, a] = gausselimPivot(A,b)
    [nA, mA] = size(A);
    [nb, mb] = size(b);
    
    if nA <> mA then
        error("gausselim: matriz no es cuadrada");
    elseif mA <> nb then
        error("gausselim: no coinciden las dimensiones de A y b");
    end
    
    // Declaramos matriz aumentada.
    a = [A b];
    n = nA;
    
    // Eliminación gaussiana con pivoteo parcial.
    for k=1:n-1
        // Encontrar el índice de la fila con el max valor absoluto en la columna k desde la fila k en adelante.
        // max devuelve el par [maxValor, indice], ignoramos el primero.
        // El índice obtenido por max() es relativo al subvector a(k:n, k), es decir, empieza en 1 en lugar de k. Por eso la segunda línea.
        [maxValor, max_row] = max(abs(a(k:n, k)));
        max_row = max_row + k - 1;
        
        // Intercambiar filas si es necesario para poner el mayor elemento
        if max_row <> k then
            temp = a(k,:);
            a(k, :) = a(max_row, :);
            a(max_row, :) = temp;
        end
        
        // Realizar eliminación gaussiana.
        for i=k+1:n
            mult = a(i, k)/a(k, k);
            a(i,k:n+1) = a(i,k:n+1) - mult*a(k,k:n+1);
        end
    end
    
    // Inicializar el vector solución
    x = zeros(n, 1);

    // Sustitución regresiva.
    x(n) = a(n, n+1)/a(n, n);
    for i = n-1:-1:1
        sumk = 0;
        for k=i+1:n
            sumk = sumk + a(i,k)*x(k);
        end
        
        x(i) = (a(i, n+1) - sumk)/a(i,i);
    end
endfunction

// Matrices del ejercicio.
A = [1 1 0 3; 2 1 -1 1; 3 -1 -1 2; -1 2 3 -1];
b = [4 1 -3 4]';
[x, a] = gausselimPivot(A,b)
disp(x);
disp(A*x);

A = [1 -1 2 -1; 2 -2 3 -3; 1 1 1 0; 1 -1 4 3];
b = [-8 -20 -2 4]';
[x, a] = gausselimPivot(A,b)
disp(x);
disp(A*x);

A = [1 1 0 4; 2 1 -1 1; 4 -1 -2 2; 3 -1 -1 2];
b = [2 1 0 -3]';
[x, a] = gausselimPivot(A,b)
disp(x);
disp(A*x);


