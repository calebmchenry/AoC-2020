import System.IO  
import Data.List

pairEquals2020 :: (Int, Int) -> Bool
pairEquals2020 (a, b) =  a + b == 2020

pairs :: [Int] -> [(Int, Int)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

multiplyPair :: (Int, Int) -> Int
multiplyPair (a,b) = a * b

solve :: String -> Int
solve = head . map multiplyPair . filter pairEquals2020. pairs . map read . lines

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solve contents
        hClose handle
