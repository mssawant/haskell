module Main where

import Phone 

main :: IO ()
main = print $ buttonPressToString dp [(6, 1), (2, 1), (6, 2), (3, 1), (2, 1), (7, 3), (11, 3), (7, 4)]
