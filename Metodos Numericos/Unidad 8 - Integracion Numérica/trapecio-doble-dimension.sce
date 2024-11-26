// Para que Scilab no muestre los warning. No proteste.
funcprot(0);

function v = Tn(fx,a,b,n)
    h = (b-a)/n;
    v = (fx(a)/2) + (fx(b)/2);
    for i = 1:n-1
        xi = a+i*h;
        v = v + fx(xi);
    end
    
    v = h*v;
endfunction

// n ab m cd
function v = trapecioDobleDimension(f,a,b,c,d,n,m)
    h = (b-a)/n;              // h para la integral de mas afuera
    deff("z=fxa(y)", "z=f(" + string(a) + ",y)");
    deff("z=fxb(y)", "z=f(" + string(b) + ",y)");
    v = Tn(fxa,c(a),d(a),m)/2 + Tn(fxb,c(b),d(b),m)/2;
    
    for i=1:n-1
        xi = a + i*h;
        deff("z=fxi(y)", "z=f(" + string(xi) + ",y)");
        v = v + Tn(fxi,c(xi),d(xi),m);
    end
    
    v = h* v;
endfunction

// Ejercicio 6
function v = uno(x,y)
    v = 1;
endfunction

function y = cx(x)
    y = -sqrt(2*x-x^2);
endfunction

function y = dx(x)
    y = sqrt(2*x-x^2);
endfunction

// Tiene que dar pi
v = trapecioDobleDimension(uno,0,2,cx,dx,100,100);
disp(v);
