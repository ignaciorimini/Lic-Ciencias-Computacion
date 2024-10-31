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


// Primer sistema.
A = [0 2 4; 1 -1 -1; 1 -1 2];
b = [0; 0.375; 0];
x0 = [0; 0; 0];
tol = 1e-2;
maxIter = 100;

[x, iter] = metodoJacobi(A, b, x0, tol, maxIter);
disp("Solución por Jacobi: ");
disp(x);
disp("Número de iteraciones: " + string(iter));

[x, iter] = metodoGaussSeidel(A, b, x0, tol, maxIter);
disp("Solución por Gauss-Seidel: ");
disp(x);
disp("Número de iteraciones: " + string(iter));

// Segundo sistema.
A = [1 -1 0; -1 2 -1; 0 -1 1.1];
b = [0; 1; 0];
x0 = [0; 0; 0];
tol = 1e-2;
maxIter = 500;

[x, iter] = metodoJacobi(A, b, x0, tol, maxIter);
disp("Solución por Jacobi: ");
disp(x);
disp("Número de iteraciones: " + string(iter));

[x, iter] = metodoGaussSeidel(A, b, x0, tol, maxIter);
disp("Solución por Gauss-Seidel: ");
disp(x);
disp("Número de iteraciones: " + string(iter));
