import System.IO  

numOfRepeats :: [String] -> Int
numOfRepeats xs =  (h * 3 `div` w ) + 1
    where h = length xs
          w = length $ head xs

duplicate :: Int -> String -> String
duplicate n str = concat $ replicate n str

repeatGrid :: Int -> [String] -> [String]
repeatGrid n str = map (duplicate n) str

adjustRow :: String -> String
adjustRow = drop 3

rotateGrid :: [String] -> [String]
rotateGrid [] = []
rotateGrid xs = (take 1 xs) ++ (map adjustRow $ rotateGrid $ drop 1 xs)

treeInPath :: String -> Bool
treeInPath str = head str == '#'

solve :: String -> Int
solve rows = length $ filter treeInPath $ rotateGrid $ repeatGrid repeats $ lines $ rows
    where repeats = numOfRepeats $ lines rows

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solve contents
        hClose handle
