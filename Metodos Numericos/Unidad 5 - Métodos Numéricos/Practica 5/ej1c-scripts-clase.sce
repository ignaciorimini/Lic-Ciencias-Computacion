function x = jacobi(A,b,x0,eps)
    n = size(A,1);
    x = x0;
    xk = x;
    suma = 0;
    cont = 0;
    
    for i=1:n
    suma = 0
        for j = 1:n
            if (i<>j)
                suma = suma + A(i,j)*xk(j)
            end
        end
    x(i) = 1/A(i,i)*(b(i)-suma)
    end
    cont = cont+1

    while (abs(norm(x-xk))> eps)
        xk = x;
        for i=1:n
            suma = 0
            for j = 1:n
                if (i<>j)
                    suma = suma + A(i,j)*xk(j)
                end
            end
            x(i) = 1/A(i,i)*(b(i)-suma)
        end
     cont = cont+1
    end
    disp(cont);
endfunction


// Gauss-Seidel
// xi^{k+1} = 1/aii (bi - sum_{j=1}^{i-1} aij xj^{k+1} 
//                      - sum_{j=i+1}^{n} aij xj^{k}
//                  )

function x = gauss(A,b,x0,eps)
    n = size(A,1);
    x = x0;
    xk = x;
    suma = 0;
    cont = 0;
    
    for i=1:n
    suma = 0
        for j = 1:n
            if (i<>j)
                suma = suma + A(i,j)*x(j)
            end
        end
    x(i) = 1/A(i,i)*(b(i)-suma)
    end
    cont = cont+1

    while (abs(norm(x-xk))> eps)
        xk = x
        for i=1:n
            suma = 0
            for j = 1:n
                if (i<>j)
                    suma = suma + A(i,j)*x(j)
                end
            end
            x(i) = 1/A(i,i)*(b(i)-suma)
        end
     cont = cont+1
    end
    disp(cont);
endfunction

// Primer sistema.
A = [0 2 4; 1 -1 -1; 1 -1 2];
b = [0; 0.375; 0];
x0 = [0; 0; 0];
tol = 1e-2;

x = jacobi(A,b,x0,tol)
disp("Solución por Jacobi: ");
disp(x);

x = gauss(A,b,x0,tol);
disp("Solución por Gauss-Seidel: ");
disp(x);

// Segundo sistema.
A = [1 -1 0; -1 2 -1; 0 -1 1.1];
b = [0; 1; 0];
x0 = [0; 0; 0];
tol = 1e-2;

x = jacobi(A,b,x0,tol)
disp("Solución por Jacobi: ");
disp(x);

x = gauss(A,b,x0,tol);
disp("Solución por Gauss-Seidel: ");
disp(x);
