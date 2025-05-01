msort :: [Int] -> [Int]
msort []    = []
msort [x]   = [x]
msort xs    = 
    let (ls, rs)    = split xs
        (ls', rs')  = (msort ls, msort rs)
    in merge (ls', rs')

split :: [Int] -> ([Int], [Int])
split []        = ([], [])
split [x]       = ([x], [])
split (x:y:zs)  =
    let (xs, ys) = split zs
    in (x:xs, y:ys)

merge :: ([Int], [Int]) -> [Int]
merge ([], ys) = ys
merge (xs, []) = xs
merge (x:xs, y:ys) =
    if x <= y
        then x : merge(xs, y:ys)
        else y : merge (x:xs, ys)