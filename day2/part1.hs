import System.IO  

replaceHyphen :: Char -> Char
replaceHyphen '-' = ' '
replaceHyphen  c   = c

countLetters :: Char -> String -> Int
countLetters c str = length $ filter (== c) str

validPass :: [String] -> Bool
validPass [quantity,letter,password] =  (&&) (count >= read min) (count <= read max)
    where count = countLetters char password
          char = head letter
          [min, max] = words $ map replaceHyphen quantity

solve :: String -> Int
solve = length . filter validPass . map words . lines

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solve contents
        hClose handle
