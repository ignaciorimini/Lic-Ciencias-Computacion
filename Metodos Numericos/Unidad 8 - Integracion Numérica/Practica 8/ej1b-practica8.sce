// Regla del trapecio aproximada
function w = trapecio(f,a,b)
    w = (f(a)+f(b)) * (b-a)/2;
endfunction

// Regla del trapecio extendida
function w = trapecio_ext(f,a,b,n)
    // Longitud de los intervalos.
    h = (b-a)/n;
    
    // Sumamos los términos extremos
    w = (f(a)+f(b))/2;
    
    // Sumamos los términos intermedios
    for i=1:(n-1)
        xi = a + h*i;
        w = w + f(xi);
    end
    
    // Multiplicamos por h
    w = h*w;
endfunction

// Regla de Simpson
function w = simpson(f,a,b)
    h = (b-a)/2;
    x1 = a + h;
    w = (f(a)+4*f(x1)+f(b)) * h/3;
endfunction

// Metod0 compuesto de Simpson
function w = simpson_compuesto(f,a,b,n)
    h = (b-a)/n;
    
    // Sumamos los valores extremos
    w = f(a)+f(b);
    
    // Sumamos los valores intermedios
    for i=1:2:n-1
        xi = a + i*h;
        w = w + 4*f(xi);
    end
    
    for i=2:2:n-1
        xi = a + i*h;
        w = w + 2*f(xi);
    end
    
    w = h/3 * w;
endfunction

// Funcion del ejercicio
function y = f2(x)
    y = x^(1/3);
endfunction

// Metod0 trapecio
w = trapecio(f2,0,0.1)
disp("Método trapecio: ", w);

w = trapecio_ext(f2,0,0.1,100)
disp("Método trapecio extendido: ", w);

// Metod0 simpson
w = simpson(f2,0,0.1)
disp("Método simpson: ", w);

w = simpson_compuesto(f2,0,0.1,100)
disp("Método simpson compuesto: ", w);

w = intg(0,0.1,f2);
disp("Integral por Scilab: ", w);

