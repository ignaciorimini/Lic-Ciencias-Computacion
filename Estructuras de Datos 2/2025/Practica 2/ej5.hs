-- Reescribir cada una de las siguientes definiciones sin usar let, where o if.

f1 x = let (y,z) = (x,x) in y
f1' x = x

greater (x,y) = if x > y
                    then True
                    else False
greater' (x,y) = x > y

f3 (x,y) = let z = x+y
            in g (z,y)
            where g (a,b) = a-b
f3' (x,y) = x
