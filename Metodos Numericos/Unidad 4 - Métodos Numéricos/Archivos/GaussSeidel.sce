//Gauss-Seidel
//xi^{k+1} = 1/aii * (bi - sum_{j=1}^{i-1} aij xj^{k+1} - sum_{j=i+1,j/=i}^{n} aij xj^{k})

function x = metodoGS(A,b,x0,eps)
    
    nA = size(A,1);
    cont = 0;
    x = x0;
    xk = x;
    
    for i = 1:nA
        suma = 0;
      
        for j = 1:i-1  
            suma = suma + A(i,j)*x(j); // uso la iter actual
        end
            
        for j = (i+1):nA
            suma = suma + A(i,j)*x(j); // uso la iter actual
        end
            
        x(i) = 1/A(i,i)*(b(i) - suma); 
    end
    
    cont = cont + 1;
    
    while ((norm(x-xk))>eps)
        
        xk = x;
    
        for i = 1:nA
        
            suma = 0;
      
            for j = 1:i-1  
                suma = suma + A(i,j)*x(j); // uso la iter actual
            end
            
            for j = i+1:nA
                suma = suma + A(i,j)*x(j); // uso la iter actual
            end
            
            x(i) = 1/A(i,i)*(b(i) - suma); 
        
        end
        
        cont = cont + 1;
        
    end
        
    disp(cont);
    
endfunction

A = [4 -1 0; -1 4 -1; 0 -1 3];
b = [15; 10; 10];
x0 = [0; 0; 0];
tol = 1e-6;
maxIter = 100;

x = metodoGS(A, b, x0, tol);
disp("Solución: ");
disp(x);
