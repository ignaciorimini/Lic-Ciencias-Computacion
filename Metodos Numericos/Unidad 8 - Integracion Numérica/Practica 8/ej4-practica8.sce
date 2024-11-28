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

// Metod0 compuesto de Simpson
function w = metodoSimpsonCompuesto(fx,a,b,n)
    h = (b-a)/n;
    
    w = fx(a)+fx(b);
    
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

// _____________________________________
// Ejercicio.
function y = f(x)
    y = 1/(x+1);
endfunction

a = 0;
b = 1.5;
n = 10;

res = integrate('f','x',a,b);
resTrapecio = metodoTrapecioCompuesto(f,a,b,n);
errTrapecio = abs(res - resTrapecio);

resSimpson = metodoSimpsonCompuesto(f,a,b,n)
errSimpson = abs(res - resSimpson);

disp("Integral real: " + string(res));
disp("Integral por trapecio compuesto: " + string(resTrapecio));
disp("Error por trapecio: " + string(errTrapecio));
disp("Integral por Simpson compuesto: " + string(resSimpson));
disp("Error por Simpson: " + string(errSimpson));
