// Metod0 del trapecio simple (sin el error)
function y = metodoTrapecio(fx,a,b)
    y = (fx(a)+fx(b)) * (b-a)/2;
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

// Ejemplo de prueba: funcion x^2 de integral x^3/3 -> integral = 0.3333 en [0,1]
function y = f(x)
    y = x.^2;
endfunction

a = 0;
b = 1;
n = 8;
res = metodoTrapecio(f,a,b);
disp("Integral por metodo del trapecio simple:", res);

res2 = metodoTrapecioCompuesto(f,a,b,n);
disp("Integral por metodo del trapecio compuesto:", res2);
