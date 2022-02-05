module Main where

import Stage3

main :: IO ()
main = initGame >>= playGame
