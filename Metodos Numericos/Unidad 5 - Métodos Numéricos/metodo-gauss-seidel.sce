function [x, iteraciones] = metodoGaussSeidel(A,b,x0,tol,maxIter)
    n = size(A, 1);     // Numero de filas de la matriz.
    x = x0;             // Vector inicial.
    iteraciones = 0;    // Contador de iteraciones.
    
    while iteraciones < maxIter
        xOld = x;       // Guardamos el valor anterior de x.
        
        // Iteramos sobre cada ecuación.
        for i=1:n
            suma = 0;
            
            // Suma de terminos anteriores a x_i, con valores nuevos.
            for j=1:i-1
                suma = suma + A(i,j)*x(j);
            end
            
            // Suma de términos posteriores a x_i, con valores viejos.
            for j=i+1:n
                suma = suma + A(i,j)*xOld(j);
            end
            
            x(i) = (b(i) - suma)/A(i,i);
        end
        
        iteraciones = iteraciones + 1;
        
        // Verificar tolerancia de error.
        if norm(x - xOld) < tol then
            return;
        end
    end
endfunction

A = [4 -1 0; -1 4 -1; 0 -1 3];
b = [15; 10; 10];
x0 = [0; 0; 0];
tol = 1e-6;
maxIter = 100;

[x, iter] = metodoGaussSeidel(A, b, x0, tol, maxIter);
disp("Solución: ");
disp(x);
disp("Número de iteraciones: " + string(iter));
