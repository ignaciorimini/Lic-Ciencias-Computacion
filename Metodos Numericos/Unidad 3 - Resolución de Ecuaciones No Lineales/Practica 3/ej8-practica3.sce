function y = newtonMultivariable(sisFunciones, vectorVariables, err, iter)
    for i=1:iter;
        J = numderivative(sisFunciones, vectorVariables);
        
        if det(J) == 0 then
            error("La matriz jacobiana no es invertible.");
        end
        
        inversaJ = inv(J);
        F = sisFunciones(vectorVariables);
        delta = -inversaJ * F;
        vectorVariables = vectorVariables + delta;
       
        if norm(delta) < err then
            break;
        end
    end
    
    y = vectorVariables;
endfunction


function Fx = sistemaFunciones(vectorVariables)
    x = vectorVariables;
    Fx(1) = x(1)^2 + x(1)*x(2)^3 - 9;
    Fx(2) = 3*x(1)^2*x(2) - 4 - x(2)^3;
endfunction


vectorInicialA = [1.2; 2.5];
vectorInicialB = [-2; 2.5];
vectorInicialC = [-1.2; -2.5];
vectorInicialD = [2; -2.5];
errorDeseado = 1e-6;
maxIteraciones = 20;

solucionA = newtonMultivariable(sistemaFunciones, vectorInicialA, errorDeseado, maxIteraciones);
solucionB = newtonMultivariable(sistemaFunciones, vectorInicialB, errorDeseado, maxIteraciones);
solucionC = newtonMultivariable(sistemaFunciones, vectorInicialC, errorDeseado, maxIteraciones);
solucionD = newtonMultivariable(sistemaFunciones, vectorInicialD, errorDeseado, maxIteraciones);

disp(solucionA);
disp(solucionB);
disp(solucionC);
disp(solucionD);

