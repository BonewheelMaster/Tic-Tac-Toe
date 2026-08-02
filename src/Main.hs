module Main where

import UI

import Control.Exception (finally)

main :: IO ()
main = do
    initScreen
    loop startSetup
    `finally`
    endScreen
    where
    loop state = do
        drawOutput state
        input <- getChar
        loop $ next input state
