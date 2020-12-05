import System.IO  
import Data.List

--    byr (Birth Year)
--    iyr (Issue Year)
--    eyr (Expiration Year)
--    hgt (Height)
--    hcl (Hair Color)
--    ecl (Eye Color)
--    pid (Passport ID)
--    cid (Country ID)

containsFields :: [String] -> Bool
containsFields fields = elem "byr" fields && elem "iyr" fields && elem "eyr" fields && elem "hgt" fields && elem "hcl" fields && elem "ecl" fields && elem "pid" fields 

isValidPassport :: String -> Bool
isValidPassport =  containsFields . map (takeWhile (/= ':')) . words

passports [] = []
passports lines = front:back 
    where front = intercalate " " $ takeWhile (/= "") lines 
          back = passports $ drop 1 $ dropWhile (/= "") lines


solve = length . filter isValidPassport . passports . lines

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $  solve contents
        hClose handle
