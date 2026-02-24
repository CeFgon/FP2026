import Language.Haskell.TH (prim)
-- - I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Num a => a -> a -> a
osszeg a b = a + b

osszeg2 :: Int -> Int -> Int
osszeg2 a b = (+) a b

kulonbseg a b = a - b
kulonbseg2 a b = (-) a b

szorzat a b = a * b

szorzat2 a b = (*) a b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = a `div` b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = div a b

osztmar a b = mod a b

osztmar2 a b = a `mod` b

-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> x = -b / a
elsoF a b = (-b) / a

-- - egy szám abszulút értékét,
abszolut n = if n < 0 then -n else n

abszolut2 n
    | n < 0 = -n
    | otherwise = n

-- - egy szám előjelét,
elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"

elojel2 n
    | n < 0 = "negativ"
    | n > 0 = "pozitiv"
    | otherwise = "nulla"

-- - két argumentuma közül a maximumot,
max1 a b = if a > b then a else b

max2 a b
    | a > b = a
    | otherwise = b

-- - két argumentuma közül a minimumot,
min1 a b = if a < b then a else b

min2 a b
    | a < b = a
    | otherwise = b

-- - egy másodfokú egyenlet gyökeit,
-- a*(x**2) + b*x + c = 0 -> a,b,c
-- delta = b**2 - 4*a*c
-- gy1 = (-b + sqrt delta) / (2*a)
-- gy2 = (-b - sqrt delta) / (2*a)

masodF a b c = if delta < 0 then error "komplex szamok" else (gy1, gy2)
    where
        delta = b**2 -4*a*c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

masodF2 a b c
    | delta < 0 = error "komplex szamok"
    | otherwise = (gy1, gy2)
    where
        delta = b**2 -4*a*c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

masodF3 a b c
    | delta < 0 = error "komplex szamok"
    | delta == 0 = [gy1]
    | otherwise = [gy1, gy2]
    where
        delta = b**2 -4*a*c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.  Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- Peldaul: (6, 7) egyenlo (7,6)-al, de (6, 7) nem egyenlo (4, 7)-el.
elempar ep1 ep2 = if (a == c && b == d) || (a == d && b == c) then True else False
    where
        (a,b) = ep1
        (c,d) = ep2

elempar2 ep1 ep2 = (a == c && b == d) || (a == d && b == c)
    where
        (a,b) = ep1
        (c,d) = ep2

-- - az n szám faktoriálisát (3 módszer),
-- 5! = 1*2*3*4*5
fakt1 0 = 1
fakt1 n = n * fakt1 (n-1)

fakt2 n
    | n < 0 = error "negativ szam"
    | n == 0 = 1
    | otherwise = n * fakt2 (n-1)

-- pelda meghivas: fakt3 1 5
fakt3 res n
    | n < 0 = -1
    | n == 0 = res
    | otherwise = fakt3 (res*n) (n-1)

-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgyokN n = [ sqrt i | i <- [1..n] ]

-- - az első n négyzetszámot,
negyzetN :: (Enum a, Floating a) => a -> [a]
negyzetN n = [i * i | i <- [1 .. n]]

-- - az első n természetes szám köbét,
kobN n = [i ^ 3 | i <- [1 .. n]]

-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzetN n = [ i | i <- [1..n], (sqrt i * sqrt i) /= i]

-- - x hatványait adott n-ig,
xhatvanyN x n = [x ^ i | i <- [1 .. n]]

-- - egy szám páros osztóinak listáját,
osztokN n = [i | i <-[1 .. n], mod i 2 == 0, n `mod` i == 0]

osztokN2 n = [i | i <- [2, 4 .. n], mod n i == 0]

-- - n-ig a prímszámok listáját,
osztok n = [i | i <-[1 .. n], mod n i == 0]

primszam n = osztok n == [1, n]

primszamokN n = [i | i <- [2..n], primszam i == True]

primszamokN2 n = [i | i <- [2..n], primszamL i]
    where
        primszamL n = osztokL n == [1, n]
        osztokL n = [i | i <- [1..n], mod n i == 0]

-- - n-ig az összetett számok listáját,
osszetettN n = [i | i <- [0..n], not (primszam i)]

-- - n-ig a páratlan összetett számok listáját,
paratlanOsszetettN n = [i | i <- [0..n], not (primszam i), mod i 2 /=0]

paratlanOsszetettN2 n = [i | i <- [3,5..n], not (primszam i)]


-- - az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a,b,c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a**2 + b **2 == c**2]

-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a' .. 'z'] [0 .. 25]

-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok1 = zip [0..5] [5,4..0]

szamok2 n = [(i,n-i) | i <- [0..n]]

szamok3 n = zip [0..n] [n,n-1 ..0]

-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
tfLs n = take n ls
    where
        ls = [True, False] ++ ls


main :: IO()
main = do
    putStrLn "x hatvany n"
    print(xhatvanyN 5 3)
    putStrLn "paros osztok 48"
    print (osztokN 48)
    putStrLn "n-ig a páratlan összetett számok listája"
    print (paratlanOsszetettN 30)
    putStrLn ("az n-nél kisebb Pitágorászi számhármasok" ++ show (pitagorasz 100))
    print betuSzam
    print szamok1
    print (szamok2 10)
    print (szamok3 10)
    putStrLn ("5 True,False: " ++ show(tfLs 5))