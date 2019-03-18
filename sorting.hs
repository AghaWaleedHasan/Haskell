--practice of all the common sorting algorithms


insert_sorted :: [Int] -> Int -> [Int]
insert_sorted = \list -> \v -> 
 case list of
  [] -> [v]
  x:xs | v>x -> x:insert_sorted xs v
  _ -> v:list

isort :: [Int] -> [Int]
isort = \list ->
 case list of
  [] -> []
  x:xs -> insert_sorted (isort xs) x  
  
selection_sort :: [Int] -> [Int]
selection_sort = \list ->
 case list of
  [] -> []
  _ -> find_min list : selection_sort(remove_one list (find_min list))

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
 | x <= y = x:merge xs (y:ys)
 | otherwise = y:merge (x:xs) ys

merge_sort :: [Int] -> [Int]
merge_sort [] = []
merge_sort [x] = [x]
merge_sort l = merge (merge_sort (front l)) (merge_sort (back l))
 where 
  front l = take ((length l) `div` 2) l
  back l = drop ((length l) `div` 2) l

quick_sort :: [Int] -> [Int]
quick_sort [] = []
quick_sort (x:xs) = (quick_sort lower) ++ [x] ++ (quick_sort upper) 
 where
  lower = [y | y <- xs, y <= x]
  upper = [y | y <- xs, y > x]
