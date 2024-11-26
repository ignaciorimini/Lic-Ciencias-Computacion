// Función que calcula los Lk(x).
// Recibe un vector x = (x1, x2, ... xk) y el numero k y retorna el polinomio Lk(x)
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

// ________________________________________________
// Función del ejercicio con operaciones vectorizadas.
function y = f(x)
    y = 1 ./ (1 + x.^2);
endfunction

// Función que recibe los extremos de un intervalo y un numero n, y devuelve
// un vector de n nodos equiespaciados en dicho intervalo.
function nodos = nodosEquiespaciados(a,b,n)
    nodos = zeros(1,n);
    for i=0:n-1
        nodos(i+1) = a + i * (abs(b-a)/(n-1));
    end
endfunction

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

// Función que recibe extremos del intervalo y numero n, y devuelve el polinomio
// interpolador de Lagrange de grado n-1.
function [px,err] = interpolarFuncionLagrange(a,b,n)
    // Calcular los nodos equiespaciados.
    nodos = nodosEquiespaciados(a,b,n);
    
    // Evaluar f en los nodos equiespaciados.
    y = zeros(1,n)
    for i=1:n
        y(i) = f(nodos(i));
    end
    
    // Polinomio interpolador de Lagrange.
    px = interpolacionLagrange(nodos,y);
    
    // Calcular error y graficarlo
    err = graficarError(a,b,f,px);
endfunction

// Definir el intervalo.
a = -5;
b = 5;
n = 12;
[px, err] = interpolarFuncionLagrange(a,b,n);
disp("Polinomio interpolador de Lagrange para n = " + string(n) + ": ");
disp(px);
