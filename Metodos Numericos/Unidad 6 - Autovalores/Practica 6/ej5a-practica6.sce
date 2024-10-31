function [zNew, autovalor, iter] = metodoPotencia(A,zOld,tol,maxIter)
    autovalor = 0;
    iter = 0;
    
    // Calculamos w^n y z^n.
    w = A*zOld;
    zNew = w/norm(w, %inf);
    
    // Calculamos el autovalor.
    [maxValor, maxIndex] = max(abs(w));
    autovalor = w(maxIndex)/zOld(maxIndex);
    zNew = w/autovalor;
    
    while (iter <= maxIter) && (norm(zNew - zOld, %inf) > tol)
        // Actualizamos zOld y calculamos el siguiente w y z.
        zOld = zNew;
        w = A*zOld;
        zNew = w/norm(w, %inf);
        
        // Calculamos el autovalor.
        [maxValor, maxIndex] = max(abs(w));
        autovalor = w(maxIndex)/zOld(maxIndex);
        zNew = w/autovalor;
        
        iter = iter + 1;
    end
endfunction

// Matrices del ejercicio.
A = [6 4 4 1; 4 6 1 4; 4 1 6 4; 1 4 4 6];
zOld = [1 1 1 1]';
tol = 10e-6;
maxiter = 500;

[autovector, autovalor, iteraciones] = metodoPotencia(A,zOld,tol,maxiter);
disp("Iteraciones: " + string(iteraciones));
disp("Autovalor:", autovalor);
disp("Autovector:", autovector);
disp("Verif: A*v = lambda*v");
disp(A*autovector);
disp(autovalor*autovector);
disp("Verif: (A - lambda*I)v = 0");
disp((A - autovalor*eye(4,4))*autovector);

B = [12 1 3 4; 1 -3 1 5; 3 1 6 -2; 4 5 -2 -1];
[autovector, autovalor, iteraciones] = metodoPotencia(B,zOld,tol,maxiter);
disp("Iteraciones: " + string(iteraciones));
disp("Autovalor:", autovalor);
disp("Autovector:", autovector);
disp("Verif: A*v = lambda*v");
disp(B*autovector);
disp(autovalor*autovector);
disp("Verif: (A - lambda*I)v = 0");
disp((B - autovalor*eye(4,4))*autovector);
