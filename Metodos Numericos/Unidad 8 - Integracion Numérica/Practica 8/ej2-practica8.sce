funcprot(0);

// Metod0 del trapecio simple (sin el error)
function y = metodoTrapecio(f,a,b)
    y = (f(a)+f(b)) * (b-a)/2;
endfunction

// Metod0 del trapecio compuesto
function y = metodoTrapecioCompuesto(fx,a,b,n)
    // Longitud de los intervalos.
    h = (b-a)/n;
    
    // Sumamos los términos extremos (f(x0) y f(xn))
    y = (fx(a)+fx(b))/2;
    
    // Sumamos los términos intermedios f(xi).
    for i=1:(n-1)
        xi = a + h*i;
        y = y + fx(xi);
    end
    
    // Multiplicamos tod0 por h.
    y = h*y;
endfunction

// _____________________________________
// Ejercicio a.
function y = f(x)
    y = 1;
endfunction

a = 1;
b = 3;
n = 4;
res = integrate('f','x',a,b);
res1 = metodoTrapecioCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio a.");
disp("Integral real: " + string(res) + "| Integral por metodo del trapecio compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio b.
function y = f(x)
    y = x^3;
endfunction

a = 0;
b = 2;
n = 4;
res = integrate('f','x',a,b);
res1 = metodoTrapecioCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio b.");
disp("Integral real: " + string(res) + "| Integral por metodo del trapecio compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio c.
function y = f(x)
    y = x*(1+x^2)^(1/2);
endfunction

a = 0;
b = 3;
n = 6;
res = integrate('f','x',a,b);
res1 = metodoTrapecioCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio c.");
disp("Integral real: " + string(res) + "| Integral por metodo del trapecio compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio d.
function y = f(x)
    y = sin(%pi*x);
endfunction

a = 0;
b = 1;
n = 8;
res = integrate('f','x',a,b);
res1 = metodoTrapecioCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio d.");
disp("Integral real: " + string(res) + "| Integral por metodo del trapecio compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");


// _____________________________________
// Ejercicio e.
function y = f(x)
    y = x*sin(x);
endfunction

a = 0;
b = 2*%pi;
n = 8;
res = integrate('f','x',a,b);
res1 = metodoTrapecioCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio e.");
disp("Integral real: " + string(res) + "| Integral por metodo del trapecio compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio f.
function y = f(x)
    y = x^2*exp(x);
endfunction

a = 0;
b = 1;
n = 8;
res = integrate('f','x',a,b);
res1 = metodoTrapecioCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio f.");
disp("Integral real: " + string(res) + "| Integral por metodo del trapecio compuesto: " + string(res1));
disp("Error: " + string(err1))
