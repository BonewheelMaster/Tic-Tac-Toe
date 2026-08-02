{-# LANGUAGE MultiWayIf #-}
module UI.Input where

import UI.State
import Game

import Data.Char
import Data.Maybe
import Prelude hiding (init)

next :: Char -> State -> State
next c s
    | isSetup s = submitSetup $ if
        | isNothing $ inputBot s -> case c' of
            'Y' -> s { inputBot = Just True }
            'N' -> s { inputBot = Just False }
            _   -> s
        | isNothing $ inputFirstPlayer s -> case c' of
            'X' -> s { inputFirstPlayer = Just X }
            'O' -> s { inputFirstPlayer = Just O }
            _   -> s
        -- This shouldn't ever happen, because we are attempting to submit every time.
        | otherwise -> submitSetup s
    | isPlaying s = if c' `elem` ['1'..'9']
        then submitPlay (toEnum $ read [c] - 1) s
        else s
    | isEnded s = if c' == 'Y' then startSetup else s
    where
    c' = toUpper c
