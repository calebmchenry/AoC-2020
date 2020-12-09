import System.IO  
import Data.List

preambleLength :: Int
preambleLength = 25

contiguousSets :: [a] -> [[a]]
contiguousSets ns = filter ((>= 2) . length ) $ ((reverse $ map reverse $ inits $ reverse ns) >>= inits)

windows :: [Int] -> [[Int]]
windows = map (take (preambleLength + 1)) . filter ((> preambleLength) . length) . reverse . map reverse . inits . reverse

notSumOfPrev :: (Eq a, Num a) => [a] -> Bool
notSumOfPrev w = length [ x + y | x <- ns, y <-ns , x /= y , x + y == n] == 0
    where n = last w
          ns = init w

invalidNumber :: [Int] -> Int
invalidNumber = last . head . filter notSumOfPrev . windows 

sumFirstAndLast :: Num a => [a] -> a
sumFirstAndLast ns = f + l
    where f = head ns
          l = last ns

solve :: String -> Int
solve str = sumFirstAndLast $ sort $ head $ filter (\x -> sum x == n) s
    where ns = map read $ lines str
          n = invalidNumber ns
          s = contiguousSets ns
    

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
