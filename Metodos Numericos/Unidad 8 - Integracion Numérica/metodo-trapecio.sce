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
