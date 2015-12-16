import Data.Char (digitToInt,intToDigit)
import Data.List (delete,foldl')

--import Prelude   (floor,ceiling)

e32::(Integral a)=>a->a
e32 s | s > 99999 = 0
      | otherwise = e32h 0 s (reverse (divisors s))

e32h::(Integral a)=>a->a->[a]->a
e32h a s []     =  e32h a (s + 1) (reverse (divisors (s + 1)))
e32h a s (d:ds) = 
    let
        c = div s d
    in
        if s > 99999
        then a
        else
            if is_pandigital (read ((show s) ++ (show d) ++ (show c)))
            then e32h (a + s) (s + 1) (reverse (divisors (s + 1)))
            else e32h a       s       ds


stringToInteger::String->Integer
stringToInteger s = num_from_list 0 ((toInteger (length s)) - 1) s

num_from_list::Integer->Integer->[Char]->Integer
num_from_list t o []     = t
num_from_list t o (x:xs) = num_from_list (t + ((toInteger (digitToInt x)) * 10^o)) (o - 1) xs


numToString::(Num t)=>t->String
numToString n = show n

is_pandigital::(Num t)=>t->Bool
is_pandigital n | (length (show n)) > 10 = False
                | otherwise             =
    let 
        m = show n
        x = ['1','2','3','4','5','6','7','8','9','0']
        y = reverse (drop (abs ((length m) - 10)) (reverse x))
        z = foldr (+) 0 [1 | a<-y, (a `elem` m)]
    in
        if z == length m
        then True
        else False

--pandigitals::(Integral i)=>[i]
--pandigitals = let ds = "1234567890"
--              in

--perms::String->[String]
--perms []                   = []
--perms xs | (length xs) > 2 = map (\x -> [(head xs)]:x) (perms (tail xs))
--                             [(head xs) : z | z <- [ perms (filter (\x -> x /= y)) | y <- (tail xs)]]
--         | otherwise       = reverse xs

--perms []                     = []
--perms xs  | (members xs) > 1 = let h = head xs
--                                   t = tail xs
--                               in  [[h] ++ t] ++ [ [h] ++ y | y <- (rotate t)] ++ [(perms z) | z <- (rotate xs)]
--          | otherwise        = [(reverse xs)]

--perms(x:xs) | (members xs) > 2 = [ x : y | y <- [(perms z) | z <- (rotate xs)]]
--            | otherwise        = reverse xs

rotate::[t]->[[t]]
rotate xs = rotate_helper 1 xs

rotate_helper::(Integral t)=>t->[a]->[[a]]
rotate_helper n (x:xs) | n < ((members xs) + 1) = [xs ++ [x]] ++ (rotate_helper (n + 1) (xs ++ [x]))
                       | otherwise             = []

digits::(Num t, Integral i)=>t->i
digits n = members (filter (\x -> not (x `elem` ".-+e")) (show n))

best::(Integral t)=>t->t->[t]->[[t]]->[t]
best c b t []     =  [c,b] ++ t
best c b t (x:xs) = 
    if (head x) > c
    then best (head x) (head (tail x)) (tail (tail x)) xs
    else best c        b               t               xs

--e27a a b = 

--e27b::(Integral t)=>t->t->t->[t]->[[t]]
--e27b a b n c = 
--    let
--        k = n^2 + (a*n) + b
--    in
--        if (is_prime k)
--        then e27b a b (n + 1) (c + 1)
--        else [c, a*b, a, b]


is_prime::(Integral t)=>t->Bool
is_prime 0 = False
is_prime 1 = False
is_prime n | (length (show n)) == 1 && (elem (head (show n)) "2357") = True
           | (elem (head (reverse (show n))) "024568")               = False
           | otherwise                                               = is_prime_h n 2

is_prime_h::(Integral t)=>t->t->Bool
is_prime_h n d | n == d    = True
               | otherwise =
                   if (mod n d) == 0
                   then False
                   else is_prime_h n (d + 1)

neg::(Num t)=>t->t
neg 0 = 0
neg n = n - n*2

divisors::(Integral t)=>t->[t]
divisors 0 = []
divisors n = [x | x <-[1 .. (div n 2)], (mod n x) == 0]


uniq::(Eq a)=>[a]->[a]
uniq []     = []
uniq (x:xs) =  
    if (count x xs) < 1
    then x : (uniq xs)
    else uniq xs

count::(Eq t)=>t->[t]->Integer
count _ []     = 0
count c (x:xs) | x == c    = 1 + count c xs
               | otherwise = count c xs

--e34::(Integral t)=>t->t->t
e34 s n | n <  3         = s
        | n >= 999999999 = s
        | otherwise      = 
    let 
        f = foldr (+) 0 (map (fac) (map (digitToInt) (show n)))
    in 
        if f == n 
        then e34 (s + n) (n + 1)
        else e34 s       (n + 1)


fac::(Integral t)=>t->t
fac n | n == 0    = 1
      | otherwise = n * fac (n - 1)

is_circular::Integer->Bool
is_circular n | n < 2     = False
              | otherwise = 
    if (length ([1 | x <- (rotations n), (is_prime x)])) == (length (rotations n))
    then True
    else False

can_circulate::Integer->Bool
can_circulate n = 
    if (length (show n)) <= 1
    then (elem (intToDigit (fromInteger n)) "2357")
    else if (length [1 | x<-"024568", (elem x (show n))]) > 0
         then False
         else True

can_truncate::(Integral i)=>i->Bool
can_truncate n =
    if (length (show n)) <= 1
      then if (elem (head (integralToDigits n)) "2357")
        then True
        else False
    else if (length [1 | x<-"024568", (elem x (tail (show n)))]) > 0
         || (length [1 | x<-"0468", (elem x (tail (show n)))]) > 0
         then False
         else True

is_truncatable::(Integral i)=>i->Bool
is_truncatable n | (length (show n)) == 1 = (elem (head (integralToDigits n)) "2357")
                 | otherwise              =
                     if (is_prime n)
                     then if is_truncatable_left n
                          then is_truncatable_right n
                          else False
                     else False


is_truncatable_right::(Integral i)=>i->Bool
is_truncatable_right n | (length (show n))  < 1 = False
                       | (length (show n)) == 1 = (elem (head (integralToDigits n)) "2357")
                       | otherwise              = if (is_prime n)
                                                  then is_truncatable_right (digitsToIntegral (tail (show n)))
                                                  else False

is_truncatable_left::(Integral i)=>i->Bool
is_truncatable_left n | (length (show n))  < 1 = False
                      | (length (show n)) == 1 = (elem (head (integralToDigits n)) "2357")
                      | otherwise              =
                           if (is_prime n)
                           then is_truncatable_left (digitsToIntegral (reverse (tail (reverse (show n)))))
                           else False

find_truncatable_primes::(Integral i)=>[i]->[i]->[i]
find_truncatable_primes []     ts                     = ts
find_truncatable_primes (n:ns) ts | (length ts) == 11 = ts
                                  | otherwise         = 
                                      if (is_truncatable n)
                                      then find_truncatable_primes ns (n:ts)
                                      else find_truncatable_primes ns ts

rotations::(Integral t)=>t->[t]
rotations n | n < 10    = [n]
            | otherwise = map (digitsToIntegral) (get_rotations (show n) (toInteger (length (show n))))

get_rotations::(Integral i)=>String->i->[String]
get_rotations s l | l < 2     = [s]
                  | otherwise = s : (get_rotations (reverse ((head s):(reverse (tail s)))) (l - 1))


-- possible states of an n^2 bit vector
-- used to determine possible states for 
-- an n x n Conway's Game of Life
possible_states::(Integral t)=>t->t
possible_states n | n == 0    = 0
                  | otherwise = foldr (+) 0 [(choose (n^2) k) | k<-[0..n^2]]

--Kronecker delta function
kronecker::(Eq t)=>t->t->Int
kronecker i j | i == j    = 1
              | otherwise = 0

--impulse unit for signal processing
impulse::(Num t)=>t->t
impulse n | n == 0    = 1
          | otherwise = 0


-- / N \
-- |   |  N choose k
-- \ k /
choose::(Integral t)=>t->t->t
choose n k | n <= 0    = 0
           | otherwise = div (product [1..n]) ((product [1..k]) * (product [1..(n-k)]))

nearestWholeNumber::(RealFrac t)=>t->t
nearestWholeNumber x | x >= (fromInteger ((floor (x / 1.0)))) + 0.5  = fromInteger (ceiling x)
                     | otherwise                                     = fromInteger (floor x)

--not mine
combination :: [a] -> Int -> [[a]]
combination _   0    = [[]]
combination []  _    = []
combination (x:xs) n = map (x:) (combination xs (n-1)) ++ combination xs n

inter x [] = [[x]] 
inter x yys@(y:ys) = [x:yys] ++ map (y:) (inter x ys)

perm []     = [[]]
perm (x:xs) = concatMap (inter x) (perm xs)
--end not mine

digitToIntegral::(Integral t)=>Char->t
digitToIntegral x = let charmap = [('0',0), ('1',1), ('2',2), ('3',3), ('4',4), ('5',5), ('6', 6), ('7',7), ('8', 8), ('9',9)]
                    in  (snd (head (filter (\y -> (fst y) == x) charmap)))

digitsToIntegral::(Integral t)=>[Char]->t
digitsToIntegral x = let charmap = [('0',0), ('1',1), ('2',2), ('3',3), ('4',4), ('5',5), ('6', 6), ('7',7), ('8', 8), ('9',9)]
                     in digitsToIntegral_helper 0 x charmap

digitsToIntegral_helper::(Integral t)=>t->[Char]->[(Char,t)]->t
digitsToIntegral_helper n []     _       = n
digitsToIntegral_helper n (x:xs) charmap = let m = fromIntegral (members xs)
                                               d = (snd (head (filter (\y -> (fst y) == x) charmap))) * (10^m)
                                           in  digitsToIntegral_helper (n + d) xs charmap

integralToDigits::(Integral t)=>t->[Char]
integralToDigits n = let charmap = [(0,'0'), (1,'1'), (2,'2'), (3,'3'), (4,'4'), (5,'5'), (6,'6'), (7,'7'), (8,'8'), (9,'9')]
                     in integralToDigits_helper n [] charmap

integralToDigits_helper::(Integral t)=>t->[Char]->[(t,Char)]->[Char]
integralToDigits_helper 0 digs _       | (members digs) == 0 = "0"
                                       | otherwise           = digs
integralToDigits_helper n digs charmap | n <= 9              = reverse ((snd (head (filter (\y -> (fst y) == n) charmap))):digs)
                                       | otherwise           = let m = (digits n) - 1
                                                                   q = (n `div` (10^m))
                                                                   d = (snd (head (filter (\y -> (fst y) == q) charmap)))
                                                               in  integralToDigits_helper (n - (q*(10^m))) (d:digs) charmap

factors::(Integral i)=>i->[i]
factors n | n == 0    = [0]
          | otherwise = reverse (factors_helper n (n `div` 2) [])

factors_helper::(Integral i)=>i->i->[i]->[i]
factors_helper n m fs | m == 0           = fs
                      | (n `mod` m) == 0 = factors_helper n (m - 1) (m:fs)
                      | otherwise        = factors_helper n (m - 1) fs

prime::(Integral i)=>i->Bool
prime n | n <= 9                                    = n `elem` [2,3,5,7]
        | (head (reverse (show n))) `elem` "024568" = False
        | otherwise                                 = prime_helper n 2 []

prime_helper::(Integral i)=>i->i->[i]->Bool
prime_helper n m fs | (length fs) >= 1                     = False
                    | m > (n `div` 2)                      = True
                    | (n `mod` m) == 0                     = False
                    | otherwise                            = prime_helper n (m + 1) fs

members::(Integral t)=>[a]->t
members [] = 0
members l  = members_helper 0 l
 
members_helper::(Integral t)=>t->[a]->t
members_helper n []     = n
members_helper n (x:xs) = members_helper (n + 1) xs

--assumes 2-D space
state_space::(Integral i)=>i->i
state_space size = foldl' (+) 0 [choose (fst y) (snd y) | y <- [(size^2, x) | x <-[0 .. size^2]]]

state_spaces::(Integral i)=>i->i->[i]
state_spaces low high = [state_space x | x <- [low .. high]]

--arbitrary dimensionality of space
state_space2::(Integral i)=>i->i->i
state_space2 size dim = foldl' (+) 0 [choose (fst y) (snd y) | y <- [(size^dim, x) | x <-[0 .. size^dim]]]

state_spaces2::(Integral i)=>i->i->i->[i]
state_spaces2 low high dim = [state_space2 x dim | x <- [low .. high]]

charToDigit c =
    case c of '0' -> 0
              '1' -> 1
              '2' -> 2
              '3' -> 3
              '4' -> 4
              '5' -> 5
              '6' -> 6
              '7' -> 7
              '8' -> 8
              '9' -> 9
              otherwise -> error "Not a digit"

myconcat::forall a. [[a]]->[a]
myconcat xs = myconcat' xs []
              where 
                  myconcat' (x:xs) ys = myconcat' xs (foldr (:) x ys)
                  myconcat' _      ys = ys

fetch x y zs = zs !! (truncate (y * sqrt(fromIntegral(length(zs))) + x))

--fetch2 xs ys = ys !! (fetch2' xs)
--               where fetch2' (z:zs) = let x = head (reverse zs)
--                                          y = reverse (tail (reverse zs))
--                                      in truncate (zipWith (*) (foldr (*) 1 y) z) + x

-- Determine the correct index for a simulated multidimensional array stored as a flat array
-- d = dimensions array e.g. [y-size,  z-size]           or [2-order-dimension, ... n-order-dimension]
-- i = indices          e.g. [x-index, y-index, z-index] or [1-order-index,     2-order-index,     ... n-order-index]
-- sum for each index of the index multiplied by the size of each higher dimension
-- e.g.
-- idxs = [1,2,1], dims = [3,3]
--         ^              *^*^
-- +         ^              *^
-- +           ^
-- midx dims idxs = (1 * 3 * 3) + (2 * 3) + 1
--               = 16th index of a flat array
midx::(Integral a)=>[a]->[a]->a
midx ds (i:is) = (foldr (*) i ds) + (midx (tail ds) is)
midx _ _       = 0
