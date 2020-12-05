import System.IO  
import Data.List

repl :: Char -> Char
repl ':' = ' '
repl c = c

splitOnColon :: [Char] -> (String, String)
splitOnColon str= (ws!!0, ws!!1)
    where ws = words $ map repl str

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True

isHexDigit :: Char -> Bool
isHexDigit '0' = True
isHexDigit '1' = True
isHexDigit '2' = True
isHexDigit '3' = True
isHexDigit '4' = True
isHexDigit '5' = True
isHexDigit '6' = True
isHexDigit '7' = True
isHexDigit '8' = True
isHexDigit '9' = True
isHexDigit 'a' = True
isHexDigit 'b' = True
isHexDigit 'c' = True
isHexDigit 'd' = True
isHexDigit 'e' = True
isHexDigit 'f' = True
isHexDigit _ = False


--    byr (Birth Year)
--    iyr (Issue Year)
--    eyr (Expiration Year)
--    hgt (Height)
--    hcl (Hair Color)
--    ecl (Eye Color)
--    pid (Passport ID)
--    cid (Country ID)

validBirthYear :: (String, String) -> Bool
validBirthYear (key, value) = key == "byr" && y >= 1920 && y <= 2002
    where y = read value

validIssueYear :: (String, String) -> Bool
validIssueYear (key, value) = key == "iyr" && y >= 2010 && y <= 2020
    where y = read value

validExpirationYear :: (String, String) -> Bool
validExpirationYear (key, value) = key == "eyr" && y >= 2020 && y <= 2030
    where y = read value

validHeight :: (String, String) -> Bool
validHeight (key, value) = key == "hgt" && validHeight
    where unit = reverse $ take 2 $ reverse value
          l = length value - 2
          height = read $ take l value
          validHeight = case unit of 
              "cm" -> height >= 150 && height <= 193
              "in" -> height >= 59 && height <= 76
              _ -> False

validHairColor :: (String, String) -> Bool
validHairColor (key, value) = key == "hcl" && validColor
    where validColor =  head value == '#' && (length $ filter isHexDigit $ drop 1 value) == (length $ drop 1 value)

validEyeColor :: (String, String) -> Bool
validEyeColor (key, value) = key == "ecl" && validColor
    where validColor = case value of  
            "amb" -> True
            "blu" -> True
            "brn" -> True
            "gry" -> True
            "grn" -> True
            "hzl" -> True
            "oth" -> True
            _ -> False

validPassportId :: (String, String) -> Bool
validPassportId (key, value)= key == "pid" && (length value == 9) && (length $ (filter isDigit value)) == 9

validCountryId :: (String, String) -> Bool
validCountryId (key, _) = key == "cid"

isValidEntry :: [Char] -> Bool
isValidEntry str = validBirthYear pair || validIssueYear pair || validExpirationYear pair || validHeight pair || validHairColor pair || validEyeColor pair || validPassportId pair || validCountryId pair
    where pair = splitOnColon str


containsFields :: [String] -> Bool
containsFields fields = elem "byr" fields && elem "iyr" fields && elem "eyr" fields && elem "hgt" fields && elem "hcl" fields && elem "ecl" fields && elem "pid" fields 

isValidPassport :: String -> Bool
isValidPassport str =  containsFields keys && length (filter isValidEntry entries) == (length entries)
    where keys = map (takeWhile (/= ':')) $ words str
          entries = words str

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
