import System.IO  
import Data.List

tripletEquals2020 :: (Int, Int, Int) -> Bool
tripletEquals2020 (a, b, c) =  a + b + c == 2020

triplets :: [Int] -> [(Int, Int, Int)]
triplets l = [(x,y,z) | x <- l , y <- l, z <- l]

multiplyTriplet :: (Int, Int, Int) -> Int
multiplyTriplet (a,b, c) = a * b * c

solve :: String -> Int
solve = head . map multiplyTriplet . filter tripletEquals2020. triplets . map read . lines

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solve contents
        hClose handle
