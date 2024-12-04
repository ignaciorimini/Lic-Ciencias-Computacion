function y = f1(x)
    y = tan(x) - x;
endfunction

function y = f2(x)
    y = log(1+2*x) - x^3;
endfunction

function y = f3(x)
    y = 1.6 + 0.99*cos(x) - x;
endfunction

function y = g1(x)
    y = tan(x);
endfunction

function y = g2(x)
    y = (log(1+2*x))^(1/3);
endfunction

function y = g3(x)
    y = 1.6 + 0.99*cos(x);
endfunction

function y = puntofijo(g,x0,err,iter)
    xn = x0;
    
    for i=1:iter;
        xn1 = g(xn);
        
        if abs(xn1 - xn) < err then
            y = xn1;
            return;
        end
        
        xn = xn1;
    end
    
    y = xn;
endfunction

res = puntofijo(g1,0.1,1e-6, 100);
disp("Solución de f1: " + string(res));

res = puntofijo(g2,0.7,1e-6, 100);
disp("Solución de f2: " + string(res));

res = puntofijo(g3,1.5,1e-6, 100);
disp("Solución de f3: " + string(res));

function n = numIteracionesPuntoFijo(g, x0, tol)
    x1 = g(x0);
    lambda = abs(cos(x0)^(-2));  // Derivada de g(x) = tan(x)
    n = ceil(log(tol * (1 - lambda) / abs(x1 - x0)) / log(lambda));
endfunction

n = numIteracionesPuntoFijo(g1, 0.1, 1e-4);
disp("Número de iteraciones necesarias: " + string(n));

