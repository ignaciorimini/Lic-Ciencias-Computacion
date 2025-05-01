spaml :: Seq Int -> Int
spaml s
    | (length s) <= 2 = length s
    | otherwise = spaml_aux s


spaml_aux :: Seq Int -> Int
spaml_aux s =
    let n = length s
        s_dif = tabulate (\i -> (nth(i+1) s) - (nth i s)) (n - 1)
        s_info = map f s_dif
        (s_red, r) = scan g (nth 0 s_info) (drop s_info 1)
        s_res = map h (append s_red (singleton r))
    in 1 + (reduce max 0 s_res)
    where
        f :: Int -> (Int, Int)
        f n = (abs n, 1)

        g :: (Int, Int) -> (Int, Int)
        g (a1, b1) (a2, b2) = 
            if a1 == a2
                then (a1, b1 + 1)
                else (a2, b2)

        h :: (Int, Int) -> Int
        h (_, b) = b


