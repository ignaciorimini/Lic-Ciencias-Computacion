data Color = RGB {red::Int, blue::Int, green::Int} deriving Show

mezclar :: Color -> Color -> Color
mezclar c1 c2 = RGB{red = avg (red c1) (red c2),
                    blue = avg (blue c1) (blue c2),
                    green = avg (green c1) (green c2)}
    where
        avg x y = div (x+y) 2

-- Prueba:
-- color1 = RGB {red=40, blue=40, green=20}
-- color2 = RGB {red=20, blue=20, green=60}
-- mezclar color1 color2 -> RGB {red = 30, blue = 30, green = 40}