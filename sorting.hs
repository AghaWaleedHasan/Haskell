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
