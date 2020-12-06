import System.IO  
import Data.List

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

solve str = head a
    where takenSeats = map seatId $ lines str
          diff = [0..1023] \\ takenSeats
          a = filter (\x -> (elem (x+1) diff) == False && (elem (x-1) diff) == False) diff

main :: IO ()
main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
