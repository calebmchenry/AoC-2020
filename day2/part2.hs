import System.IO  
import Data.Bool

replaceHyphen :: Char -> Char
replaceHyphen '-' = ' '
replaceHyphen  c   = c

validPass :: [String] -> Bool
validPass [quantity,letter,password] =  (&&) ((&&) (len > pos1) (len > pos2)) ((||) ((&&) a $ not b) ((&&) b $ not a))
    where [pos1, pos2] = map ( subtract 1) $ map read $ words $ map replaceHyphen quantity :: [Int]
          char = head letter
          a = password!!pos1 == char
          b = password!!pos2 == char
          len = length password

solve :: String -> Int
solve = length . filter validPass . map words . lines

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solve contents
        hClose handle
