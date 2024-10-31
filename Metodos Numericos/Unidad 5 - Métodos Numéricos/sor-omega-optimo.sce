// Recibe una matriz definida positiva y tridiagonal.
function w = omegaOptimo(A)
    n = size(A,1);
    Tj = eye(n,n) - diag(1./diag(A))*A;
    autovalores = spec(Tj);
    radioEspectral = max(abs(autovalores));
    w = 2/(1 + sqrt(1 - radioEspectral^2));
endfunction

A = [4 -1 0; -1 4 -1; 0 -1 3];  // Matriz definida positiva y tridiagonal
w = omegaOptimo(A);
disp("Omega óptimo: " + string(w));
