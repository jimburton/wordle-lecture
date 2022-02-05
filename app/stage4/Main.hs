module Main where

import Stage4

main :: IO ()
main = initGame >>= playGame
