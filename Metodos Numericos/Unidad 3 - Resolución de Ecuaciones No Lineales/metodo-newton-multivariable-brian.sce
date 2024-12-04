function y = metNewton(f,x0,iter,tol)
    J = numderivative(f,x0);
    x1 = x0 - (inv(J)*f(x0))';
    i = 1;
    
    while (norm(x1 - x0) > tol) && (i < iter)
        x0 = x1;
        J = numderivative(f,x0);
        x1 = x0 - (inv(J)*f(x0))';
        i = i + 1;
    end
    
    y = x1;
endfunction

function Fx = sistemaFunciones(vectorVariables)
    x = vectorVariables;
    Fx(1) = 1 + x(1)^2 - x(2)^2 + exp(x(1))*cos(x(2));
    Fx(2) = 2*x(1)*x(2) + exp(x(1))*sin(x(2));
endfunction

// Probar funcion:
vectorInicial = [-1, 4];
errorDeseado = 1e-6;
maxIteraciones = 5;
solucion = metNewton(sistemaFunciones, vectorInicial, errorDeseado, maxIteraciones);
disp(solucion);
