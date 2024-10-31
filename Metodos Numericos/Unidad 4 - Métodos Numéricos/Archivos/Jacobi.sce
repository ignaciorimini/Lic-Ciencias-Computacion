//Jacobi
//xi^{k+1} = 1/aii * (bi - sum_{j=1,j/=i}^{n} aij xj^{k})

function x = metodoJacobi2(A,b,x0,eps)
    
    nA = size(A,1);
    cont = 0;
    x = x0;
    xk = x;
    
    for i = 1:nA
        
      suma = 0;
      
      for j = 1:nA  
      
        if(j<>i) then
           suma = suma + A(i,j)*xk(j);
        end
      
      end  
        
       x(i) = 1/A(i,i)*(b(i) - suma); 
        
    end
    
    cont = cont + 1;
    
    while ((norm(x-xk))>eps)
        
        xk = x;
    
        for i = 1:nA
        
            suma = 0;
      
            for j = 1:nA  
      
                if(j<>i) then
                    suma = suma + A(i,j)*xk(j);
                end
      
            end  
        
            x(i) = 1/A(i,i)*(b(i) - suma); 
        
        end
        
        cont = cont + 1;
        
    end
        
    disp(cont);
    
endfunction

A = [4 -1 0; -1 4 -1; 0 -1 3];
b = [15; 10; 10];
x0 = [0; 0; 0]; // Vector inicial
tol = 1e-6;     // Tolerancia
maxIter = 100;  // Número máximo de iteraciones

x = metodoJacobi2(A, b, x0, tol);
disp("Solución: ");
disp(x);
