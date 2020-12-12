import System.IO  

isFloor :: Char -> Bool
isFloor = (== '.')

isChair :: Char -> Bool
isChair = (/= '.')

isOccupied :: Char -> Bool
isOccupied = (== '#');

isEmpty :: Char -> Bool
isEmpty = (== 'L')

inBounds :: Foldable t => [t a] -> (Int, Int) -> Bool
inBounds xss (r,c) = rInbounds && cInbounds
    where rInbounds = r >= 0 && (r < (length xss))
          cInbounds = c >= 0 && (c < (length (xss!!0)))

getChair :: [[a]] -> (Int, Int) -> a
getChair xss (r,c) = xss!!r!!c

adjacent :: (Eq a, Eq b, Num a, Num b) => a -> b -> [(a, b)]
adjacent r c = zip b d
    where delta = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
          a = unzip $ filter (/= (0,0)) $ delta
          b = map (+r) $ fst a   
          d = map (+c) $ snd a   

adjacentWithCenter :: (Num a, Num b) => a -> b -> [(a, b)]
adjacentWithCenter r c = zip b d
    where delta = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
          a = unzip delta
          b = map (+r) $ fst a   
          d = map (+c) $ snd a   

desireable :: [[Char]] -> Int -> Int -> Bool
desireable xss r c = all emptyChair $ map chair $ filter (inBounds xss) $ adjacentWithCenter r c
    where chair = getChair xss
          emptyChair x = isFloor x || isEmpty x

tooOccupied :: [[Char]] -> Int -> Int -> Bool
tooOccupied xss r c = 4 <= length (filter occupiedChair $ map chair $ filter (inBounds xss) $ adjacent r c)
    where chair = getChair xss
          occupiedChair x = isChair x && isOccupied x

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

simulate xss = group (length (xss!!0)) $ map (update xss) $ c
    where xs = [0..(length xss - 1)]
          ys = [0..(length (xss!!0) - 1)]
          c = [ (x,y) | x <- xs, y <-ys]

solve xss = if (prevCount) == (nextCount) then prevCount  else solve $ simulate xss
    where countOccupiedSeats = length . filter isOccupied . ( id =<<)
          prevCount = countOccupiedSeats xss
          nextCount = countOccupiedSeats $ simulate xss

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solve $ lines $ contents
        hClose handle
