function [x, iteraciones] = metodoSOR(A,b,x0,w,tol,maxIter)
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
            
            // Suma de terminos posteriores a x_i, con valores viejos.
            for j=i+1:n
                suma = suma + A(i,j)*xOld(j);
            end
            
            // Actualización de x(i) usando SOR
            x(i) = (1-w)*xOld(i) + (w/A(i,i)) * (b(i) - suma);
        end 
        
        iteraciones = iteraciones + 1;
        
        // Verificar tolerancia de error.
        if norm(x - xOld) < tol then
            return;
        end
    end
endfunction

// Si w = 1 -> Gauss-Seidel: 11 iteraciones.
// Si w = 1.1 -> 9 iteraciones.
// Si w = 0.75 -> 22 iteraciones
// Si w = omegaOptimo -> 8 

// Matriz tridiagonal definida positiva.
A = [4 -1  0  0; -1  4 -1  0; 0 -1  4 -1; 0  0 -1  3];   
b = [15; 10; 10; 10];
x0 = [0; 0; 0; 0];
tol = 1e-6;
maxIter = 100;
w = 1.1;

[x, iter] = metodoSOR(A, b, x0, w, tol, maxIter);
disp("Solución: ");
disp(x);
disp("Número de iteraciones: " + string(iter));

w = omegaOptimo(A);
[x, iter] = metodoSOR(A, b, x0, w, tol, maxIter);
disp("Omega óptimo: " + string(w));
disp("Solución: ");
disp(x);
disp("Número de iteraciones: " + string(iter));
