import System.IO  

slope :: (Int, Int) -> Double
slope (right, down) = fromIntegral(right) / fromIntegral(down)

numOfRepeats :: Double -> [String] -> Int
numOfRepeats m xs =  ceiling $ (h * m / w) + 1 ::Int
    where h = fromIntegral $ length xs :: Double
          w = fromIntegral $ length $ head xs :: Double

duplicate :: Int -> String -> String
duplicate n str = concat $ replicate n str

repeatGrid :: Int -> [String] -> [String]
repeatGrid n str = map (duplicate n) str

adjustRow :: Int -> String -> String
adjustRow right = drop right

rotateGrid :: (Int, Int) -> [String] -> [String]
rotateGrid _ [] = []
rotateGrid (right, down) xs = (take 1 xs) ++ (map (adjustRow right) $ rotateGrid (right, down) $ drop down xs)

treeInPath :: String -> Bool
treeInPath str = head str == '#'

solve :: String -> (Int, Int) -> Int
solve rows m = length $ filter treeInPath $ rotateGrid m $ repeatGrid repeats $ lines $ rows
    where repeats = numOfRepeats (slope m) $ lines rows

slopes = [(1, 1), (3, 1) , (5, 1) , (7, 1), (1, 2)]

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ product $ map (solve contents) slopes
        hClose handle
