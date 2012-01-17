module Brainhuck.Util.List (
  replace
) where
  
replace :: [a] -> Int -> (a -> a) -> [a]
replace xs i f = take i xs ++ [f $ xs !! i] ++ drop (i + 1) xs