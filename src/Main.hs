module Main where

import DayThirteen (Time, partTwo)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    value <- partTwo
    print value
