function x = metodoGS(A, b, x0, eps)
    nA = size(A, 1);
    x = x0;
    
    cont = 0;
    
   
    while 1
        xk = x;
        
       
        for i = 1:nA
            suma = A(i, :) * x - A(i, i) * x(i);  
            x(i) = (b(i) - suma) / A(i, i);
        end
        
        cont = cont + 1;
        
        
        if norm(x - xk) <= eps
            break;
        end
    end
    
    disp(['Número de iteraciones: ', string(cont)]);
end
