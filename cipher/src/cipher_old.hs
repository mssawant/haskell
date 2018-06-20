module Cipher where

import Data.Char

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

--noSpace :: String -> String
--noSpace [] = []
--noSpace s  = dropWhile (== ' ') s

vigenereCipherEncode :: String -> String -> String
vigenereCipherEncode [] []         = []
vigenereCipherEncode [] (y:ys)     = []
vigenereCipherEncode (x:xs) t@(y:ys) = if x /= ' ' && y /= ' ' then
                                          if y >= 'A' && y <= 'Z' then
                                             caesarCipherEncode (x : []) ((ord y) - 65) ++ vigenereCipherEncode xs ys
                                          else 
                                             caesarCipherEncode (x : []) ((ord y) - 97) ++ vigenereCipherEncode xs ys
                                       else if xs /= [] && ys == [] then
                                               vigenereCipherEncode xs t
                                            else
                                               " " ++ vigenereCipherEncode xs ys
