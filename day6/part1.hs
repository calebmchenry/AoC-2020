import System.IO  
import Data.List

groupAnswers :: String -> String
groupAnswers =  concat . words

answers :: [[Char]] -> [[Char]]
answers [] = []
answers lines = front:back 
    where front = intercalate " " $ takeWhile (/= "") lines 
          back = answers $ drop 1 $ dropWhile (/= "") lines

solve =  sum . map length . map nub . map sort .map groupAnswers . answers . lines

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
