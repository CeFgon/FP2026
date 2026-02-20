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
-- - az első n négyzetszámot,
-- - az első n természetes szám köbét,
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
-- - x hatványait adott n-ig,
-- - egy szám páros osztóinak listáját,
-- - n-ig a prímszámok listáját,
-- - n-ig az összetett számok listáját,
-- - n-ig a páratlan összetett számok listáját,
-- - az n-nél kisebb Pitágorászi számhármasokat,
-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.