import System.IO  
import Data.List

variations :: (Eq a, Num a, Num p) => a -> p
variations 0 = 1
variations 1 = 1
variations 2 = 2
variations 3 = 4
variations 4 = 7
              

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = front:back 
    where front = takeWhile (/= x) xs
          back = splitOn x $ drop 1 $ dropWhile (/= x) xs

joltDiffProduct ds = ones * threes
    where ones = (length $ filter (== 1) ds)
          threes = length $ filter (== 3) ds 

joltDiff (a,b) = b - a

windows ls = zip r l
    where r = ls
          l = drop 1 $ ls <> [last ls + 3]

solve = product . map (variations . length) . splitOn 3 . map joltDiff . windows . (0:) . sort . map read . lines
    

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
