module Phone where

import Data.Char

data DaPhone = DaPhone { layout :: [String] } deriving (Eq, Show)

isPresent :: String -> Char -> Bool
isPresent [] _ = False
isPresent s c = if c == (s !! 0) then
                   True
                else
                   isPresent (drop 1 s) c

getPos :: String -> Char -> Int -> Int
getPos [] _ _ = 0
getPos s c n = if c /= (s !! 0) then
                 if (s !! 0) /= ' ' then
                    getPos (drop 1 s) c (n + 1)
                 else
                    getPos (drop 1 s) c n
               else
                 n
               

pressCount :: String -> Char -> (Char, Int)
pressCount [] _ = ('0', 0)
pressCount s c = (s !! 0, getPos s c 0)

charToButtonPressCount :: DaPhone -> Char -> (Char, Int)
charToButtonPressCount (DaPhone []) _ = error "Invalid input"
charToButtonPressCount d c = if isPresent ((layout d) !! 0) c then
                           pressCount ((layout d) !! 0) c
                        else
                           charToButtonPressCount (DaPhone (drop 1 (layout d))) c


noSpace :: String -> String
noSpace []     = [] 
noSpace (x:xs) = if x == ' ' then
                    noSpace xs
                 else
                    x : (noSpace xs)

change :: Char -> Char
change c = chr ((ord c) + 26 + 7 - 1)

pressCountToChar :: String -> Int -> Char
pressCountToChar s n = let ns = noSpace s in
                           ns !! (mod n (length ns))
                                  
                           

buttonPressToChar :: DaPhone -> (Int, Int) -> Char
buttonPressToChar d b = let c = pressCountToChar ((layout d) !! ((fst (b)) - 1)) (snd b) in
                            if c == '0' then
                               ' '
                            else
                               change c


buttonPressToString :: DaPhone -> [(Int, Int)] -> String
buttonPressToString (DaPhone []) _ = error "Invalid input"
buttonPressToString d []           = []
buttonPressToString d (x:xs)       = (buttonPressToChar d x) : (buttonPressToString d xs)
--pressCountToChar ((layout d) !! (b - 1)) n 
 

dp = DaPhone ["1", "2 ABC", "3 DEF", "4 GHI", "5 JKL", "6 MNO", "7 PQRS", "8 TUV", "9 WXYZ", "* ^", "0 + _", "# . ,"]
