// MetodO de eliminación Gauss-Jordan con pivoteo parcial.
function [x, a] = gaussjordan(A,b)
    [nA, mA] = size(A);
    [nb, mb] = size(b);
    
    if nA <> mA then
        error("gaussjordan: matriz no es cuadrada");
    elseif mA <> nb then
        error("gaussjordan: no coinciden las dimensiones de A y b");
    end
    
    // Declaramos matriz aumentada.
    a = [A b];
    n = nA;
    
    // Gauss-Jordan con pivoteo parcial.
    for k=1:n
        [maxValor, max_row] = max(abs(a(k:n, k)));
        max_row = max_row + k - 1;
        
        // Intercambiar filas si es necesario para poner el mayor elemento
        if max_row <> k then
            temp = a(k,:);
            a(k, :) = a(max_row, :);
            a(max_row, :) = temp;
        end
        
        // Escalamos la fila del pivote para hacer a(k, k) = 1.
        a(k, :) = a(k, :)/a(k, k);
        
        // Hacer ceros en todas las demás filas en la columna k.
        for i = 1:k-1
            mult = a(i, k);
            a(i, :) = a(i, :) - mult * a(k, :);
        end
        for i = k+1:n
            mult = a(i, k);
            a(i, :) = a(i, :) - mult * a(k, :);
        end
    end
    
    x = a(:, n+1);
endfunction

// Ejemplo de aplicación
A2 = [0 2 3; 2 0 3; 8 16 -1]
b2 = [7 13 -3]'

[x2,a2] = gaussjordan(A2,b2)
disp(x2)
disp(a2)
