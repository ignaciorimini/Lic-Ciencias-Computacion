function circ(r,x,y)
    xarc(x-r,y+r,2*r,2*r,0,360*64);
endfunction

function Gers(A)
    [n,m] = size(A);
    centros = diag(A);                  // Vector de centros de círculos.
    radios = sum(A, 'c')-abs(centros);  // Vector de radios de círculos.
    
    // Coordenadas para generar el rectángulo que muestre todos los círculos.
    // Esquina inferior izquierda.
    xmin = round(min(centros - radios) - 1);
    ymin = round(min(-radios) - 1);
    
    // Esquina superior derecha.
    xMax = round(max(centros + radios) + 1);
    yMax = round(max(radios) + 1);
    
    // Generamos el rectángulo y dibujamos.
    rectangulo = [xmin, ymin, xMax, yMax];
    plot2d(0,0,-1,"031","",rectangulo);
    xgrid();
    
    // Generamos un círculo por cada fila de A.
    for i=1:n
        radioCirc = radios(i);
        centroCirc = centros(i);
        circ(radioCirc,centroCirc,0);
    end
endfunction

function CircGersValor(A)
    [n,m] = size(A);
    centros = diag(A);
    radios = sum(A, 'c')-abs(centros);
    
    // Coordenadas para generar el rectángulo que muestre todos los círculos.
    // Esquina inferior izquierda.
    xmin = round(min(centros - radios) - 1);
    ymin = round(min(-radios) - 1);
    
    // Esquina superior derecha.
    xMax = round(max(centros + radios) + 1);
    yMax = round(max(radios) + 1);
    
    // Generamos el rectángulo y dibujamos los autovalores;
    rectangulo = [xmin, ymin, xMax, yMax];
    autovalores = spec(A);
    plot2d(real(autovalores),imag(autovalores),-1,"031","",rectangulo);
    xgrid();
    
    // Generamos un círculo por cada fila de A.
    for i=1:n
        radioCirc = radios(i);
        centroCirc = centros(i);
        circ(radioCirc,centroCirc,0);
        
        // Mostramos los autovalores como texto.
        xstring(real(autovalores(i)), imag(autovalores(i)), string(autovalores(i)));
    end
    

endfunction

// Matriz de ejemplo.
A = [3 2 1; 2 3 0; 1 0 3];
// CircGersValor(A);

A = [4.75 2.25 -0.25; 2.25 4.75 1.25; -0.25 1.25 4.75];
CircGersValor(A)
