function x = determinante(A)
    [n,m] = size(A);
    
    if n <> m then
        error("determinante: la matriz no es cuadrada");
    end
    
    // Eliminación gaussiana.
    for k=1:n-1
        if A(k,k) == 0 then
            error("determinante: la matriz es singular");
        end
        for i=k+1:n
            mult = A(i,k)/A(k,k);
            A(i,k) = 0;
            A(i,k+1:n) = A(i,k+1:n) - mult*A(k,k+1:n);
        end
    end
    
    // Calcular determinante.
    x = prod(diag(A));
endfunction

// Matriz de ejemplo.
A = [3 2 1; 1 1 1; 2 3 1];  // det(A) = -3
B = [1 2 3; 4 5 6; 7 8 9];  // det(B) = 0 

detA = determinante(A);
detB = determinante(B);

disp("Determinante de A: " + string(detA));
disp("Determinante de B: " + string(detB));
