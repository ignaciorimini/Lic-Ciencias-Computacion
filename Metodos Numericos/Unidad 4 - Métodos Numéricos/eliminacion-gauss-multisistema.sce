function [X,Aaum] = gaussElimGenerico(A,B)
    [nA,mA] = size(A);
    [nB,mB] = size(B);
    
    if nA <> mA then
        error("gaussElimTri: la matriz no es cuadrada");
    end
    
    if mA <> nB then
        error("gaussElimTri: dimensiones incompatibles entre A y b");
    end
    
    // Escribimos la matriz aumentada [A|B].
    Aaum = [A B];
    n = nA;
    
    // Eliminación gaussiana sin pivoteo.
    for k=1:n-1
        for i=k+1:nA
            mult = Aaum(i,k)/Aaum(k,k);
            Aaum(i,k) = 0;
            Aaum(i,k+1:n+mB) = Aaum(i,k+1:n+mB) - mult*Aaum(k,k+1:n+mB);
        end
    end
    
    // Sustitución regresiva.
    X = zeros(n,n);
    X(nA, 1:nA) = Aaum(nA,nA+1:nA+nB) ./ Aaum(nA,nA);   // Ultima fila de la matriz X.
    
    for k=nA-1:-1:1
        sumatoria = Aaum(k,k+1:nA)*X(k+1:nA,1:nB);
        X(k,1:nB) = (Aaum(k,nA+1:nA+nB) - sumatoria)./Aaum(k,k);
    end
    
endfunction

A = [1 2 3;  3 -2 1; 4 2 -1];
B = [14 9 -2; 2 -5 2; 5 19 12];
[X, a] = gaussElimGenerico(A,B);
disp(a);
disp(X);

B = [1 0 0; 0 1 0; 0 0 1];
[inversa, a] = gaussElimGenerico(A,B);
disp("Inversa de A: ", inversa);
// Verificar haciendo inversa*A = I
