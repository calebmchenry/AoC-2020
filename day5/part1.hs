import System.IO  

half :: ([Int], String) -> Int
half (seats, []) = head seats
half (seats, pass) =  case head pass of
        'F' -> half (take l2 seats, tail pass)
        'B' -> half (drop l2 seats, tail pass)
        'L' -> half (take l2 seats, tail pass)
        'R' -> half (drop l2 seats, tail pass)
        where l2 = (length seats) `div` 2

seatId :: String -> Int
seatId bs = half([0..len],bs)
    where len = 2^(length bs)

solve = maximum . map seatId . lines

main :: IO ()
main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
