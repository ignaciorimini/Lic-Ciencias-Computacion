function y = f5(x)
    y = (0.25 * x.^2) - sin(x);
endfunction

x = -1.5:0.1:3;
plot(x, f5(x));
xlabel("x");
ylabel("y");
xtitle("Gráfico de f5(x)");
xgrid();

// Según el gráfico las raíces están cerca de x = 0 y x = 1.9
