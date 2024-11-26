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


// Ejemplos de prueba
n = 4;
Tn = polinomioChebyshev(4);
disp("Polinomio de chebyshev: ", Tn);

raices = raicesPolChebyshev(n);
disp("Raíces del polinomio de Chebyshev T_" + string(n) + ":");
disp(raices);
raices = raicesPolChebyshev2(n);
disp("Raíces del polinomio de Chebyshev 2 T_" + string(n) + ":");
disp(raices);

// Definir la función a interpolar
function y = f(x)
    y = sin(x);
endfunction

p = interpolacionChebyshevLagrange(f,n)
disp("Polinomio interpolante:", p);

res1 = horner(p, 0.243);
res2 = f(0.243);
disp("P(0.243): ", res1);
disp("f(0.243): ", res2);
