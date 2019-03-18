--random utility functions to practice folding in haskell


sumList :: [Int] -> Int
sumList = \list -> foldr (+) 0 list

prodList :: [Int] -> Int
prodList = \list -> foldr (*) 1 list

sumLength :: [[a]] -> Int
sumLength [] = 0
sumLength (x:xs) = length x + sumLength xs

sumL l = sum (map length l)

is_even x = (mod x 2 == 0)

even_items l = filter is_even l 

even_squares l = map square (filter is_even l) where square x = (*) x 2

mylength :: [Int] -> Int
mylength l = foldr f 0 l where f x y = y + 1

find_min l = foldr1 min l 
