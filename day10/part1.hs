import System.IO  
import Data.List

joltDiffProduct ds = ones * threes
    where ones = (length $ filter (== 1) ds) + 1
          threes = length $ filter (== 3) ds 

joltDiff (a,b) = b - a

windows ls = zip r l
    where r = ls
          l = drop 1 $ ls <> [last ls + 3]

solve = joltDiffProduct . map joltDiff . windows . sort . map read . lines
    

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
