import Data.List

lsT3 = [("mari", "mari@gmail.com", 100), ("mari", "mari@gmail.com", 150)]

myMax ls = foldl op [] ls
    where
        op res k
            | k3 > r3 = [k]
            | k3 == r3 = k : res
            | otherwise = res
                where
                    (r1,r2,r3) = head res
                    (k1,k2,k3) = k

fibonacciAux2 n = reverse $ fst $ foldl op ([], (0, 1)) [1 .. n]
    where
        op (ls, (a, b)) k = (a : ls, (b, a+b))

fibonacciAux (a, b) = a : fibonacciAux (b, a + b)

ls::[(String, String)]
ls = [("bori", "bori@gmail.com"), ("pisti", "pisti@gmail.com"), ("mari", "mari@gmail.com")]

ins fg k [] = [k]
ins fg x (k : ve)
    | fg x k = k : ins fg x ve
    | otherwise = x : k : ve

myElem_ x ls = foldl (op x) False ls
    where
        op x res k = if x == k then True || res else res

myElem x ls = foldr (op x) False ls
    where
        op x k res = if x == k then True || res else res

myFilter fg ls = foldr (op fg) [] ls
    where
        op fg k res = if fg k then k : res else res

myMap fg ls = foldr (op fg) [] ls
    where
        op fg k res = fg k : res

--megforditja a listat
myMap_ fg ls = foldl (op fg) [] ls
    where
        op fg res k = fg k : res

mySum ls = foldl op 0 ls
    where
        op res k = res + k

mySum1 ls = foldl1 op ls
    where
        op res k = res + k

