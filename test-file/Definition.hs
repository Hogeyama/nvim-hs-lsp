
module Definition where

import Definition2 (exportedFunc)

main :: IO ()
main = do
  print $ sum' $ []
  exportedFunc

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

