// Metod0 del trapecio compuesto
function y = metodoTrapecioCompuesto(fx,a,b,n)
    h = (b-a)/n;
    
    y = (fx(a)+fx(b))/2;
    
    for i=1:(n-1)
        xi = a + h*i;
        y = y + fx(xi);
    end
    
    y = h*y;
endfunction

// ___________________________
// Ejercicio.
function y = f(x)
    y = 1/x;
endfunction

a = 1;
b = 2;
n = 409;

resReal = log(2);
resAprox = metodoTrapecioCompuesto(f,a,b,n);
err = abs(resReal - resAprox);
disp("ln(2) = " + string(resReal) + " | Trapecio = " + string(resAprox) + " | Error = " + string(err));

