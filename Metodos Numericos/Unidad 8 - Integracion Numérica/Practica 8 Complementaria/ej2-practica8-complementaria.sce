// Metod0 compuesto de Simpson
function w = metodoSimpsonCompuesto(fx,a,b,n)
    h = (b-a)/n;
    
    // Sumamos los valores extremos
    w = fx(a)+fx(b);
    
    // Sumamos los valores intermedios
    for i=1:2:n-1
        xi = a + i*h;
        w = w + 4*fx(xi);
    end
    
    for i=2:2:n-1
        xi = a + i*h;
        w = w + 2*fx(xi);
    end
    
    w = h/3 * w;
endfunction

// ___________________________________
// Ejercicio.
function y = f(x)
    y = exp(x)*sin(x);
endfunction

a = 1;
b = 3;
n = 16;

resReal =integrate('f','x',a,b);
resAprox = metodoSimpsonCompuesto(f,a,b,n);
err = abs(resReal - resAprox);
disp("Real = " + string(resReal) + " | Simpson = " + string(resAprox) + " | Error = " + string(err));
