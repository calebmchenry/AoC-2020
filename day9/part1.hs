import System.IO  
import Data.List

preambleLength :: Int
preambleLength = 25

windows :: [Int] -> [[Int]]
windows = map (take (preambleLength + 1)) . filter (\x -> length x > preambleLength ) . reverse . map reverse . inits . reverse

notSumOfPrev :: (Eq a, Num a) => [a] -> Bool
notSumOfPrev w = length [ x + y | x <- ns, y <-ns , x /= y , x + y == n] == 0
    where n = last w
          ns = init w

solve :: String -> Int
solve = last . head . filter notSumOfPrev . windows . map read . lines 
    

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
