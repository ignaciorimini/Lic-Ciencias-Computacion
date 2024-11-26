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

// Item a
function y = fa(x)
    y = 1/x;
endfunction

w = trapecio_ext(fa,1,3,4)
disp("Item A Trapecio Compuesto: ", w);

w = intg(1,3,fa);
disp("Item A Scilab: ", w)

// Item b
function y = fb(x)
    y = x^3;
endfunction

w = trapecio_ext(fb,0,2,4);
disp("Item B Trapecio Compuesto: ", w);

w = intg(0,2,fb);
disp("Item B Scilab: ", w);

// Item c
function y = fc(x)
    y = x*((1+x^2)^(1/2));
endfunction

w = trapecio_ext(fc,0,3,6);
disp("Item C Trapecio Compuesto: ", w);

w = intg(0,3,fc);
disp("Item C Scilab: ", w);

// Item d
function y = fd(x)
    y = sin(%pi*x);
endfunction

w = trapecio_ext(fd,0,1,9);
disp("Item D Trapecio Compuesto: ", w);

w = intg(0,1,fd);
disp("Item D Scilab: ", w);

// Item e
function y = fe(x)
    y = x*sin(x);
endfunction

w = trapecio_ext(fe,0,2*%pi,8);
disp("Item E Trapecio Compuesto: ", w);

w = intg(0,2*%pi,fe);
disp("Item E Scilab: ", w);

// Item f
function y = ff(x)
    y = x^2 * %e^x;
endfunction

w = trapecio_ext(ff,0,1,8);
disp("Item F Trapecio Compuesto: ", w);

w = intg(0,1,ff);
disp("Item F Scilab: ", w);
