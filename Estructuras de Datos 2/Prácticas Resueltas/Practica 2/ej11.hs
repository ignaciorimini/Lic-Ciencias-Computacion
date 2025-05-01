-- Inferir de ser posible, los tipos de las siguientes funciones:
-- (puede suponer que sqrt :: Float -> Float)

modulus :: Floating a => [a] -> a
modulus = sqrt . sum . map (^2)

vmod :: Floating a => [[a]] -> [a]
vmod [] = []
vmod (v:vs) = modulus v : vmod vs