function U = primerFactCholesky(A)
    n = size(A,1);
    
    // Primer columna de U.
    a = A(2:n,1);
    
    // Submatriz de esquina inferior derecha.
    M = eye(n-1,n-1) - a*a';    // M = I - aa^t
    R22 = chol(M);              // M = R22^t R22 -> Cholesky
    
    U = [1, a'; zeros(n-1,1), R22];
endfunction

function [U, nuevaMatriz] = segundaFactCholesky(A)
    n = size(A,1);
    a = A(2:n,1);
    nuevaMatriz = [eye(n-1,n-1), a; a' 1];
    
    // Construir U.
    raiz = sqrt(1 - a'*a);
    U = [eye(n-1,n-1), a; zeros(1,n-1) raiz];
endfunction

// Construir matriz del ejercicio.
a = [0.2; 0.1; 0.3; 0.4; 0.1];
n = size(a,"r");
A = [1, a'; a, eye(n,n)];

U = primerFactCholesky(A);
disp("Matriz A:", A);
disp("Matriz U:", U);
disp("Matriz U^tU:", U'*U);


//[U,nuevaMatriz] = segundaFactCholesky(A);
//disp("Matriz A:", nuevaMatriz);
//disp("Matriz U:", U);
//disp("Matriz U^tU:", U'*U);
