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

function [autoPot, autoSpec, dif, iteraciones] = comparaAutovalor(A)
    [n, m] = size(A);
    zOld = eye(n,1);
    tol = 1e-2;
    maxIter = 500;
    
    // Aproximación del autovalor dominante con metod0 de la potencia.
    [autovector, autoPot, iteraciones] = metodoPotencia(A,zOld,tol,maxIter);
    
    // Maximo autovalor dominante usando spec.
    autoSpec = max(spec(A));
    
    // Diferencia entre autovalores.
    dif = abs(autoSpec - autoPot);
endfunction

A = [12 1 3 4; 1 -3 1 5; 3 1 6 -2; 4 5 -2 -1];
[autovalorPotencia, autovalorSpec, dif, iter] = comparaAutovalor(A);
disp("Autovalor potencia: ", autovalorPotencia);
disp("Autovalor spec: ", autovalorSpec);
disp("Diferencia: ", dif);
disp("Iteraciones: ", iter);
