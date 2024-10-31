function x = metodoGS(A, b, x0, eps)
    nA = size(A, 1);
    cont = 0;
    x = x0;
    xk = x;
    
    suma = A(1, 2:nA) * x(2:nA);
    x(1) = (b(1) - suma) / A(1, 1);
    
    for i = 2:nA-1
        suma = A(i, 1:i-1) * x(1:i-1) + A(i, i+1:nA) * x(i+1:nA);
        x(i) = (b(i) - suma) / A(i, i);
    end
    
    suma = A(nA, 1:nA-1) * x(1:nA-1)
    x(nA) = (b(nA) - suma) / A(nA, nA);
        
    while (norm(x - xk) > eps)
        xk = x;
        
        suma = A(1, 2:nA) * x(2:nA);
        x(1) = (b(1) - suma) / A(1, 1);
        
        for i = 2:nA-1
            suma = A(i, 1:i-1) * x(1:i-1) + A(i, i+1:nA) * x(i+1:nA);
            x(i) = (b(i) - suma) / A(i, i);
        end
        
        suma = A(nA, 1:nA-1) * x(1:nA-1)
        x(nA) = (b(nA) - suma) / A(nA, nA);
        
        cont = cont + 1;
    end
    
    disp(cont);
end
