module ListSeq where
import Seq

newtype ListSeq a = L [a] deriving Show

instance Seq ListSeq where
  emptyS = L []
  singletonS x = L [x]
  consS x (L xs) = L (x : xs)
  tabulateS f n = L (map f [0 .. n-1])
  fromList xs = L xs
  toList (L xs) = xs
  lengthS (L xs) = length xs
  nthS (L xs) i = xs !! i
  firstS (L xs) = xs !! 0
  mapS f (L xs) = L (map f xs)
  zipS (L xs) (L ys) = L (zip xs ys)
  reverseS (L xs) = L (reverse xs)
  scanS f e (L xs) = (L (init accs), last accs)
    where accs = scanl f e xs
  takeS n (L xs) = L (take n xs)
  dropS n (L xs) = L (drop n xs)
  appendS (L xs) (L ys) = L (xs ++ ys)
  filterS p (L xs) = L (filter p xs)
  reduceS f e (L xs) = foldl f e xs
  showtS (L [])  = EMPTY
  showtS (L [x]) = ELT x
  showtS (L xs)  =
    let len = length xs
        half = len `div` 2
        (left, right) = splitAt half xs
    in NODE (L left) (L right)
  flattenS (L xss) = L (concatMap (\(L xs) -> xs) xss)


