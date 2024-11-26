function v = trapecioExtendido(f,a,b,c,d)
    h = (b-a)*(d-c)/4;
    v = h * (f(c,a) + f(d,a) + f(c,b) + f(d,b));
endfunction

function v = trapecioCompuestoSimple()
endfunction

function y = f(x,y)
    y = sin(x+y);
endfunction

y = trapecioExtendido(f,0,2,0,1);
disp("Integral con trapecio extendido: " + string(y));
