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
