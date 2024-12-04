function y = f3(x)
    y = exp(-x) - x.^4;
endfunction

x = -2:0.01:2; 
plot(x, f3(x));
xlabel("x");
ylabel("y");
xtitle("Gráfico de f3(x)");
xgrid();

// Según el gráfico podemos ver que la raíces están cerca de -1.5 y 0.7 aproximadamente.
// MetodO de la secante.
// deff("y=g(x)", "y=exp(-x)-x^4");
// res = secante(g,-2,-1,0.0001,100); -> -1.4296061
// res = secante(g,0,1,0.0001,100); -> 0.8155200
