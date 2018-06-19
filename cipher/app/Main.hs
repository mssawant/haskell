module Main where

import Cipher

main :: IO ()
main = do
        putStrLn ("caesar cipher for ABC and shift=3 is " ++  caesarCipherDecode "ABC" 3)
        putStrLn ("carsar cipher for XYZ and shift=3 is: " ++ caesarCipherDecode "XYZ" 3)
        putStrLn ("vigenere cipher MEET AT DOWN with key string ALLY AL LYAL is " ++ vigenereCipherEncode "MEET AT DOWN" "ALLY AL LYAL")
