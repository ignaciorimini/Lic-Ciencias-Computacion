{-
Dadas las siguientes definiciones:

-- Esta función toma una lista de listas y las concatena en una sola lista. Básicamente, "aplana" una lista de listas: join [[1, 2], [3, 4], [5]]: [1,2,3,4,5]
join :: [[a]] -> [a]
join [] = []
join (xs:xss) = xs ++ join xss

-- La función singleton toma un elemento y lo convierte en una lista que contiene solo ese elemento.
singleton :: a -> [a]
singleton x = [x]

______________________________________________
a) Demostrar id = join . map singleton

P(xs): id xs = join (map singleton xs)

Demostraremos esta propiedad utilizando inducción estructural sobre listas.
Dada una propiedad P sobre listas, para que valga P(xs) para todo xs :: [a],
- probamos P([])
- probamos que si vale P(xs), entonces vale P(x:xs) también.

-----
Caso xs = []
    = join (map singleton [])       <map def>
    = join []                       <join 1era def>
    = []                            <id def>
    = id []

Luego, la propiedad vale para el caso base xs = [].

-----
Caso inductivo: supongamos que la propiedad vale para P(xs) y probemos para P(x:xs):
    = join (map singleton (x:xs))               <def map>
    = join (singleton x ++ map singleton xs)    <singleton def>
    = join ([x] ++ map singleton xs)            <Lema: distributiva>
    = join [x] ++ join (map singleton xs)       <join 2da def>
    = [x] ++ join [] ++ join (map singleton xs) <join 1era def>
    = [x] ++ [] ++ join (map singleton xs)      <++ def>
    = [x] ++ join (map singleton xs)            <HI>
    = [x] ++ id xs
    = [x] ++ xs
    = (x:xs)                                    <id def>
    = id (x:xs)

Luego, la propiedad vale para el caso inductivo.
Finalmente, la propiedad vale para todo xs :: [a].


______________________________________________
b) Demostrar join . join = join . map join

P(xss): join (join xss) = join (map join xss)

Demostraremos esta propiedad mediante inducción estructural sobre listas.
Dada una propiedad P definida sobre listas xs :: [[a]], para probar que vale P(xs) para toda xs:
- probamos que vale P([]).
- probamos que si vale P(xss), entonces también vale P(xs:xss).

-----
Caso xs = []
    = join (join [])            <join 1era def>
    = join []                   <join 1era def>
    = []
    
    Luego,
    = join (map join [])        <map def>
    = join []                   <join 1era def>
    = []

Y como coinciden las igualdades entonces la propiedad vale para el caso base.

-----
Caso inductivo: supongamos que vale P(xss) (HI) y probemos la propiedad para P(xs:xss):
    = join (join (xs:xss))                  <join 2da def>
    = join (xs ++ join xss)                 <join 2da def>
    = xs ++ join (join xss)
    
    = join (map join (xs:xss))              <def map>
    = join (join xs ++ map join xss)        <join 2da def>
    = join (xs ++ join [] ++ map join xss)  <join 1era def>
    = join (xs ++ [] ++ map join xss)
    = join (xs ++ map join xss)             <join 2da def>
    = xs ++ join (map join xss)             <HI>
    = xs ++ join (join xss)

Luego, como las igualdades coinciden, la propiedad se cumple para el caso inductivo.
Finalmente, la propiedad vale para toda xs :: [[a]].

-}