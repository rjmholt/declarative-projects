module Main where

import Proj1 (pickBestGuess, initState)

main :: IO ()
main = print (pickBestGuess initState initState)
