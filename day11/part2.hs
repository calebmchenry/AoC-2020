import System.IO  

isOccupied :: Char -> Bool
isOccupied = (== '#');

getChair :: [[a]] -> (Int, Int) -> a
getChair xss (r,c) = xss!!r!!c

visibleAdjacent :: Foldable t => [t a] -> Int -> Int -> [[(Int, Int)]]
visibleAdjacent xss r c = [n, e, s, w, nw, ne, se, sw]
    where w = reverse $ filter (/= (r,c)) $ map (\x -> (r,x)) $ [0..c]
          e = filter (/= (r,c)) $ map (\x -> (r,x)) $ [c..(length (xss!!0) - 1)]
          n = reverse $ filter (/= (r,c)) $ map (\x -> (x,c)) $ [0..r]
          s = filter (/= (r,c)) $ map (\x -> (x,c)) $ [r..(length xss - 1)]
          nw = zip ( map fst n) ( map snd w)
          ne = zip ( map fst n) ( map snd e)
          sw = zip (map fst s) ( map snd w)
          se = zip (map fst s) (map snd e)

desireable :: [[Char]] -> Int -> Int -> Bool
desireable xss r c = 0 == length (filter (isOccupied . head) $ filter (\x -> length x /= 0) $ map (dropWhile (== '.')) $ map (map (getChair xss)) $ visibleAdjacent xss r c)

tooOccupied :: [[Char]] -> Int -> Int -> Bool
tooOccupied xss r c = 5 <= length (filter (isOccupied . head) $ filter (\x -> length x /= 0) $ map (dropWhile (== '.')) $ map (map (getChair xss)) $ visibleAdjacent xss r c)

update :: [[Char]] -> (Int, Int) -> Char
update xss (r,c) = newState
    where chair = xss!!r!!c
          newState = case chair of 
              'L' -> if desireable xss r c then '#' else 'L'
              '#' -> if tooOccupied xss r c then 'L' else '#'
              a -> a

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

simulate :: [[Char]] -> [[Char]]
simulate xss = group (length (xss!!0)) $ map (update xss) $ c
    where xs = [0..(length xss - 1)]
          ys = [0..(length (xss!!0) - 1)]
          c = [ (x,y) | x <- xs, y <-ys]

solve :: [[Char]] -> Int
solve xss = if (prevCount) == (nextCount) then prevCount  else solve $ simulate xss
    where countOccupiedSeats = length . filter isOccupied . ( id =<<)
          prevCount = countOccupiedSeats xss
          nextCount = countOccupiedSeats $ simulate xss

main :: IO ()
main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solve $ lines $ contents
        hClose handle
