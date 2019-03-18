--code written on notepad and compiled with ghci

find_min :: [Int] -> Int
find_min = \list ->
 case list of
  [] -> error "Can't be empty"
  [x] -> x
  x:xs | x > find_min xs -> find_min xs
  x:_ -> x

remove_one :: [Int] -> Int -> [Int]
remove_one = \list -> \v ->
 case list of
  [] -> error "Element not found"
  x:xs | v==x -> xs
  x:xs -> x:remove_one xs v
