function [zNew, autovalor, iter] = metodoPotencia(A,zOld,tol,maxIter)
    autovalor = 0;
    iter = 0;
    
    // Calculamos w^n y z^n.
    w = A*zOld;
    zNew = w/norm(w, %inf);
    
    // Calculamos el autovalor.
    [maxValor, maxIndex] = max(abs(w));
    autovalor = w(maxIndex)/zOld(maxIndex);
    
    while (iter <= maxIter) && (norm(zNew - zOld, %inf) > tol)
        // Actualizamos zOld y calculamos el siguiente w y z.
        zOld = zNew;
        w = A*zOld;
        zNew = w/norm(w, %inf);
        
        // Calculamos el autovalor.
        [maxValor, maxIndex] = max(abs(w));
        autovalor = w(maxIndex)/zOld(maxIndex);
        
        iter = iter + 1;
    end
endfunction

// Ejemplo.
A = [9 1 -2 1; 0 8 1 1; -1 0 7 0; 1 0 0 1];
zOld = [1 1 1 1]';
zOld = [1 2 3 4]';
tol = 1e-5;
maxiter = 100;

[autovector, autovalor, iteraciones] = metodoPotencia(A,zOld,tol,maxiter);

disp("Iteraciones: " + string(iteraciones));
disp("Autovalor:", autovalor);
disp("Autovector:", autovector);
disp("Verif: A*v = lambda*v");
disp(A*autovector);
disp(autovalor*autovector);
disp("Verif: (A - lambda*I)v = 0");
disp((A - autovalor*eye(4,4))*autovector);
