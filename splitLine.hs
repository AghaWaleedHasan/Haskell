-- this returns the sum of the length of each string in a list of strings
sumLength l = foldr (+) 0 (map length l) 


--takes the first n words as long as the length of n words is less than the passed integer value
first :: [String] -> Int -> [String]
first = \list -> \v ->
 case list of 
  x:y:xs | (length x) < v && v >= 0 -> [x] ++ (first (y:xs) (v - (length x)))
  otherwise -> []


--splits a list of strings by the given length 
splitLine :: [String] -> Int -> ([String], [String])
splitLine string v 
 | (sumLength string) < v = (string, [])
 | (sumLength string) > v = (first string v, second)
 where
  second = drop (length (first string v)) string
