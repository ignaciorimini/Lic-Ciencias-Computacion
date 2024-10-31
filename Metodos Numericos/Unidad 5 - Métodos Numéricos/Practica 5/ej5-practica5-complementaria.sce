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

// Sistema del ejercicio.
A = [10 -1 2 0; -1 11 -1 3; 2 -1 10 -1; 0 3 -1 8];
b = [6 25 -11 15]';
x0 = [0 0 0 0]';
tol = 1e-5;
maxIter = 1000;

// 16 iteraciones.
[x, iter] = metodoJacobi(A, b, x0, tol, maxIter);
disp("Número de iteraciones por Jacobi: " + string(iter));
disp("Solución: ");
disp(x);

// 7 iteraciones.
[x, iter] = metodoGaussSeidel(A, b, x0, tol, maxIter);
disp("Número de iteraciones por Gauss-Seidel: " + string(iter));
disp("Solución: ");
disp(x);
