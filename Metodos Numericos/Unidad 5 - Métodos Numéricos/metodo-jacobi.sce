function [x, iteraciones] = metodoJacobi(A,b,x0,tol,maxIter)
    n = size(A, 1);     // Numero de filas de la matriz.
    x = x0;             // Vector inicial.
    xNew = x;           // Inicializar el nuevo vector de solucion.
    iteraciones = 0;    // Contador de iteraciones.
    
    while iteraciones < maxIter
        // Calcular nueva iteracion de xk.
        for i=1:n
            suma = 0;
            
            for j=1:i-1
                suma = suma + A(i,j)*x(j);
            end
 
            for j=i+1:n
                suma = suma + A(i,j)*x(j);
            end
            
            xNew(i) = (b(i) - suma)/A(i,i);
        end
        
        iteraciones = iteraciones + 1;
        
        // Verificar tolerancia de error.
        if norm(xNew - x) < tol then
            x = xNew;
            return;
        end
        
        // Actualizar x para siguiente iteración.
        x = xNew;
    end
endfunction

A = [4 -1 0; -1 4 -1; 0 -1 3];
b = [15; 10; 10];
x0 = [0; 0; 0];
tol = 1e-6;
maxIter = 100;

[x, iter] = metodoJacobi(A, b, x0, tol, maxIter);
disp("Solución: ");
disp(x);
disp("Número de iteraciones: " + string(iter));
