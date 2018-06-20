module Cipher where

import Data.Char
import Data.List.Split
import Data.List (intersperse)

encode :: Int -> Int -> Int
encode l n = if l >= 65 && l <= 90 then
                     65 + (mod ((l + n) - 65) 26)
                  else
                     97 + (mod ((l + n) - 97) 26)

decode :: Int -> Int -> Int
decode l n = if l >= 65 && l <= 90 then
                65 + (mod ((l - n - 1) - 90) 26)
             else
                97 + (mod ((l - n - 1) - 122) 26)

caesarCipherEncode :: String -> Int -> String
caesarCipherEncode [] _ = []
caesarCipherEncode (x:xs) n = let d = ord x in
                                if isLetter x then
                                   [chr (encode d n)] ++ caesarCipherEncode xs n
                                else
                                   error "Invalid input"

caesarCipherDecode :: String -> Int -> String
caesarCipherDecode [] _ = []
caesarCipherDecode (x:xs) n = let d  = ord x in
                                if isLetter x then
                                   [chr (decode d n)] ++ caesarCipherDecode xs n
                                else
                                   error "Invalid input"

letterToInt :: Char -> Int
letterToInt c = if c >= 'A' && c <= 'Z' then
                   (ord c) - 65
                else
                   (ord c) - 97

vEncode :: String -> String -> String
vEncode [] [] = []
vEncode [] (y:ys) = []
vEncode (x:xs) (y:ys) = caesarCipherEncode (x : []) (letterToInt y) ++ vEncode xs (ys ++ y : [])

cipherStringsKeys :: [String] -> [String] -> [String]
cipherStringsKeys [] []         = []
cipherStringsKeys [] (y:[])     = []
cipherStringsKeys (x:[]) (y:[]) = (vEncode x y) : []
cipherStringsKeys (x:[]) (y:ys) = (vEncode x y) : []
cipherStringsKeys (x:xs) (y:[]) = (vEncode x y) : cipherStringsKeys xs (y : [])
cipherStringKeys (x:xs) (y:ys)  = (vEncode x y) : cipherStringsKeys xs (ys ++ y : [])

vigenereCipherEncode :: String -> String -> String
vigenereCipherEncode [] [] = []
vigenereCipherEncode [] t  = []
vigenereCipherEncode s []  = s
vigenereCipherEncode s t   = let ciphers = cipherStringsKeys (splitOn " " s) (splitOn " " t) in
                                 concat $ intersperse " " ciphers
                            --if (drop 1 (dropWhile (/= ' ') t)) == "" then
                               -- (takeWhile (== ' ') s) ++ vEncode (takeWhile (/= ' ') (dropWhile (== ' ') s)) t ++
                               -- vigenereCipherEncode (dropWhile (/= ' ') s) t
                            -- else
                              --  (takeWhile (== ' ') s) ++ vEncode (takeWhile (/= ' ') (dropWhile (== ' ') s)) (takeWhile (/= ' ') t) ++
                                --vigenereCipherEncode (drop 1 (dropWhile (/= ' ') s)) (dropWhile (/= ' ') t)
                                       --if x /= ' ' && y /= ' ' then
                                         --    caesarCipherEncode (x : []) (letterToInt y) ++ vigenereCipherEncode xs (ys ++ y : [])
                                       --else if x == ' ' && y /= ' ' then
                                         --   " " ++ vigenereCipherEncode xs (y : ys)
                                       --else --if x /= ' ' && y == ' ' then
                                         --   vigenereCipherEncode (x : xs) ys
                                       --else if xs /= [] && ys == [] then
                                         --      vigenereCipherEncode xs t
                                           -- else
                                             --  " " ++ vigenereCipherEncode xs ys


checkStr :: [String] -> [String] -> [String]
checkStr [] [] = []
checkStr [] y = []
checkStr (x:xs) (y:ys) = (x) : (y) : checkStr xs (ys ++ y : [])
