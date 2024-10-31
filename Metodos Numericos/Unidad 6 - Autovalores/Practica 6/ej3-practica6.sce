function ej3()
    for k=0:10
        e = 0.1*k
        A = [1 -1 0; -2 4 -2; 0 -1 1+e];
        p = poly(A, 'x');   // Devuelve el polinomio caracteristico.
        raices = roots(p);
        autovalores = spec(A);
        
        disp("Raices (roots): ", raices);
        disp("Autovalores (spec):", autovalores);
    end
endfunction

ej3();
