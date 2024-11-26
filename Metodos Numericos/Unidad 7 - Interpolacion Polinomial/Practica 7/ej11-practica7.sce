function y = LK(x,k)
    n = length(x);              // Tamaño del vector x.
    r = [x(1:k-1) x(k+1:n)];    // Vector igual a x pero sin xk.
    p = poly(r, "x", "roots");  // Vector a partir de r pero raices (x-x1)(x-x2)... (numerador)
    pk = horner(p, x(k));       // Evalua p(xk) que es el denominador.
    y = p / pk;                 // Construimos Lk(x).
endfunction


// Función que calcula el polinomio de interpolación de Lagrange p(x).
// Recibe un vector x = (x1, x2, ... xn) y un vector y = (y1, y2, ... yn),
// estos valores son los puntos (x1, y1), (x2, y2), ... (xn, yn)
function z = interpolacionLagrange(x,y)
    n = length(x);
    pol = 0;
    
    for k=1:n
        pol = pol + (LK(x,k)*y(k));
    end
    
    z = pol;
endfunction


// Función que construye el polinomio de Chebyshev de grado n (dado).
function Tn = polinomioChebyshev(n)
    if n < 0 then
        error("polinomioChebyshev: n debe ser positivo");
    end
    
    x = poly(0,"x");
    
    if n == 0 then
        Tn = 1;
    elseif n == 1 then
        Tn = x;
    else
        // Relación de recurrencia: Tn(x)=2*x*Tn-1(x) - Tn-2(x)
        Tn_2 = 1;
        Tn_1 = x;
        for k = 2:n
            Tn = 2*x*Tn_1 - Tn_2;
            Tn_2 = Tn_1;
            Tn_1 = Tn;
        end
    end
endfunction


// Función que recibe un grado n y devuelve las raíces del polinomio de Chebyshev de grado n.
function x = raicesPolChebyshev(n)
    if n <= 0 then
        error("raicesPolChebyshev: n debe ser un entero positivo");
    end
    
    // Inicializamos vector de raíces con todos ceros.
    x = zeros(1, n);
    
    for k=1:n
        x(k) = cos(((2*k - 1)*%pi)/(2*n));
    end
endfunction


// Función que recibe un grado n y devuelve las raíces del polinomio de Chebyshev de grado n, construyendo el polinomio. 
function x = raicesPolChebyshev2(n)
    if n <= 0 then
        error("raicesPolChebyshev: n debe ser un entero positivo");
    end
    
    p = polinomioChebyshev(n);
    x = roots(p);
endfunction

//_______________________________________________
// Función g del ejercicio.
function y = g(t)
    y = cos(t);
endfunction

// Datos del ejercicio.
a = 0;
b = %pi/2;
n = 4;

// Funcion para interpolar con nodos = raices del polinomio de Chebyshev.
function Pn = interpolacionChebyshevLagrange(gx,a,b,n)
    // Calculamos nodos de Chebyshev.
    x = raicesPolChebyshev(n);
    
    // Recalculamos las raices en el intervalo [a,b].
    t = ((b+a) + x.*(b-a))/2;
    disp(t);
    
    // Evaluamos g en los nodos transformados.
    y = zeros(1, n);
    y = gx(t);
    
    // Llamamos a la función que construye el polinomio de interpolación de Lagrange.
    Pn = interpolacionLagrange(t,y);
endfunction

// Interpolamos g en el intervalo [a,b] con n nodos.
p = interpolacionChebyshevLagrange(g,a,b,n)
disp("Polinomio interpolante:", p);
