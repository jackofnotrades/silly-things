-- / N \
-- |   |  N choose k
-- \ k /
choose::(Integral t)=>t->t->t
choose n k | n <= 0    = 0 
           | otherwise = div (product [1..n]) ((product [1..k]) * (product [1..(n-k)]))
