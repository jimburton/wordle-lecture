module Main where

import Stage5

main :: IO ()
main = initGame >>= playGame
