import Data.List (sort)
import Data.List (group)
import Data.Foldable
import Data.Ord
-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,
parosNegyzet n = [x ** 2 | x <- [2, 4 .. n * 2]]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,
fel2 n = take n (aux 1)
    where
        aux i = ls i ++ aux (i + 1)
        ls i = replicate i i
-- nem jo

fel2_2 n = aux n
    where
        aux 1 = replicate 1 1
        aux i = aux (i - 1) ++ replicate i i

-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,
fel3 n = aux n
    where 
        aux 1 = replicate 1 2
        aux i = aux (i - 1) ++ replicate i (i * 2)

fel3_2 n i
    | i /= n = replicate i (i*2) ++ fel3_2 n (i + 1)
    | otherwise = replicate i (i * 2)

-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,
fel4 n = [n, n - 1 .. 1] ++ [1 .. n]

-- - váltakozva tartalmazzon True és False értékeket,
fel5 n = take n ls
    where
        ls = [True, False] ++ ls

-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.
fel6 n = take n ls
    where
        ls = [0, 1, -1] ++ ls
        

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,
osztok n = foldl (\res x -> if mod n x == 0 then res + 1 else res) 0 [1 .. n]

osztok2 n = length [ i | i <- [1..n], mod n i == 0]

osztok3 n = foldl (\res x -> if mod n x == 0 then res + 1 else res) 1 [1 .. div n 2]

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlanOsztok n = maximum [i | i <- [1 .. n], mod n i == 0, odd i]

maxParatlanOsztok2 n = last [i | i <- [1 .. n], mod n i == 0, odd i]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decP x p
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

decPSzam x p = myLength (decP x p)
    where
        myLength [] = 0
        myLength (_ : ls) = 1 + myLength ls

decPSzam2 x p = length (decP x p)

decPSzam3 x p =foldl (\res 1 -> res + 1) 0 (decP x p)

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
decPMax x p = myMaximum (decP x p)
    where
        myMaximum [e1] = e1
        myMaximum (e1 : e2 : ls)
            | e1 > e2 = myMaximum (e1 : ls)
            | otherwise = myMaximum (e2 : ls)

decPMax2 x p = maximum (decP x p)

decPMax3 x p = foldl1 (\e1 e2 -> if e1 > e2 then e1 else e2) (decP x p)

-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.
fibo a b = filter (\x -> x>a && x<b) (fibo2 0 1 0)
    where
        fibo2 _ b1 res
            | res < b = res : fibo2 b1 res (res + b1)
            | otherwise = [res]

fibo3 a b = dropWhile(< a) . takeWhile (< b) $ fiboLs 0 1 0
    where
        fiboLs _ b1 res = res : fiboLs b1 res (res + b1)

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
atlag ls = (sum ls) / fromIntegral (length ls)

atlagPozitiv ls = atlag [x | x <- ls, x > 0]

atlagPozitiv2 ls = atlag . filter(> 0) $ ls

atlag2 ls = (sum ls1) / fromIntegral (length ls1)
    where
        ls1 = filter (> 0) ls

-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
listaN ls n = [i | (idx, i) <- zip [1..] ls, mod i n == 0]

listaN2 ls n i
    | i - 1 >= length ls = []
    | mod i n == 0 = ls !! (i-1) : listaN2 ls n (i + 1)
    | otherwise = listaN2 ls n (i + 1)

listaN3 ls n = map snd $ filter (\x -> mod (fst x) n == 0) (zip [1..] ls)

-- - tükrözi egy lista elemeit,
tukroz ls = reverse ls

tukroz2 ls = map (\x -> read x::Int) $ map (reverse . show) ls

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl (\acc d -> acc * 10 + d) 0

tukroz3 ls = map (digitsToNumber . tukorSzam) ls
    where
        tukorSzam x
            | x < 10 = [x]
            | otherwise = mod x 10 : tukorSzam (div x 10)

-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
maxElemPoz ls = [idx | (idx,i) <- zip [1..] ls, i == myMax]
    where
        myMax = maximum ls

maxElemPoz2 (x : ls) = foldl aux (x, [1]) (zip ls [2..])
    where
        aux (currentMax, positions) (elem, i)
            | elem > currentMax = (elem, [i])
            | elem == currentMax = (elem, positions ++ [i])
            | otherwise = (currentMax, positions)

-- - meghatározza egy lista leggyakrabban előforduló elemét.
elof ls = map (\x -> (x, length x)) $ (group . sort) ls

elof2 ls = maxElofElem
     where
        maxElofSzam = maximum $ map length $ (group . sort) ls
        ls2 = map (\x -> (head x, length x)) $ (group . sort) ls
        maxElofElem = filter (\x -> snd x == maxElofSzam) ls2

leggyakoribb [] = error "ures lista"
leggyakoribb ls = head $ maximumBy (comparing length) $ group $ sort ls