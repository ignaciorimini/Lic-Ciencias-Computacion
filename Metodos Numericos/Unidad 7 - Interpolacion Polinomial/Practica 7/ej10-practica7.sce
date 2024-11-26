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


// Las raices del polinomio de chebyshev nos devuelve los nodos de interpolación óptimos, ahora usamos estos nodos para construir el polinomio interpolante con Lagrange.
// Función que recibe una función f que queremos interpolar y la cantidad de nodos de interpolación.
function Pn = interpolacionChebyshevLagrange(f,n)
    // Calculamos nodos de Chebyshev.
    x = raicesPolChebyshev(n);
    
    // Evaluamos f en los nodos.
    y = zeros(1,n);
    y = f(x);
    
    // Llamamos a la función que construye el polinomio de interpolación de Lagrange.
    Pn = interpolacionLagrange(x,y);
endfunction


//________________________________________
// Ejercicio
// Definir la función a interpolar.
function y = f(x)
    y = exp(x);
endfunction

// Polinomio de Chebyshev T4(x)
Tn = polinomioChebyshev(4);
disp("Polinomio de chebyshev: ", Tn);

raices = raicesPolChebyshev(4);
disp("Raíces del polinomio de Chebyshev: ");
disp(raices);

// Polinomio interpolante P3(x) usando las raices de T4(x)
p = interpolacionChebyshevLagrange(f,4)
disp("Polinomio interpolante:", p);

// Función que grafica error.
function err = graficarError(a,b,fx,px)
    puntos = linspace(a,b,100);
    err = fx(puntos) - horner(px,puntos);
    
    // Graficar el error.
    plot(puntos, err, 'r');
    title("Error de interpolación ");
    xlabel("x");
    ylabel("Error f(x) - Pn(x)");
endfunction

err = graficarError(-1,1,f,p);
