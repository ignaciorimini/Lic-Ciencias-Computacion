// Función que resuelve un sistema triangular superior.
function x = resSisTS(A,b)
    // Calculamos tamaño de A.
    [n,m] = size(A);
    
    // Verificamos que la matriz ingresada sea cuadrada.
    if n <> m then
        error("resSisTS: la matriz debe ser cuadrada");
    end
    
    // tril(A) devuelve la parte triangular inferior de A, le restamos la diagonal de A y verificamos si la matriz resultante es nula.
    if sum(tril(A) - diag(diag(A))) <> 0 then
        error("resSisTS: la matriz no es triangular superior");
    end
    
    // Verificación de ceros en la diagonal.
    if (prod(diag(A)) == 0) then
        error("resSisTS: La matriz tiene ceros en la diagonal");
    end
    
    // Resolvemos el sistema realizando sustitución regresiva.
    x = zeros(n, 1);
    for i=n:-1:1
        sumatoria = 0;
        for j=i+1:n
            sumatoria = sumatoria + A(i,j)*x(j);
        end
        
        x(i) = (b(i) - sumatoria)/A(i,i);
    end
endfunction

function x = resSisTI(A,b)
    [n,m] = size(A);
    
    if n <> m then
        error("resSisTI: la matriz ingresada no es cuadrada");
    end
    
    if sum(triu(A) - diag(diag(A))) <> 0 then
        error("resSisTI: la matriz ingresada no es triangular inferior");
    end
    
    if prod(diag(A)) == 0 then
        error("resSisTI: la matriz ingresada tiene ceros en la diagonal");
    end
    
    // Resolvemos el sistema realizando sustitución progresiva.
    x = zeros(n,1);
    for i=1:n
        sumatoria = 0;
        for j=1:i-1
            sumatoria = sumatoria + A(i,j)*x(j);
        end
        
        x(i) = (b(i) - sumatoria)/A(i,i);
    end
endfunction

A = [1 2 1; 0 -2 1; 0 0 0.5];
b = [0 3 0.5]';
x = resSisTS(A,b);
disp(x);

A = [3 0 0; 2 -1 0; 4 1 5];
b = [9 -1 13]';
x = resSisTI(A,b);
disp(x);

