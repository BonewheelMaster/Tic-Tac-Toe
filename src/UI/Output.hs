{-# LANGUAGE MultiWayIf #-}
module UI.Output where

import Game
import UI.State

import Data.List
import Data.Maybe
import System.IO

draw :: State -> String
draw s
    | isSetup   s = if
        | isNothing $ inputBot s -> "Do you want to play a bot (it will be O)? [Y/N]"
        | isNothing $ inputFirstPlayer s -> "Who goes first? [X/O]"
        -- This shouldn't ever happen, because input attempts to submit every cycle.
        | otherwise -> "Loading"
    | isPlaying s = "Turn: " <> show (turn $ board s)
        <> "\n"
        <> drawBoard (board s)
    | isEnded s   = mconcat
        [ maybe "No one" show (winner s) <> " won!\n"
        , drawBoard (lastBoard s)
        , "\n\n"
        , "Press y to go again."
        ]

drawBoard :: Board -> String
drawBoard b = drawGrid $ reverse <$> toGrid (drawPosition b . toEnum <$> [8,7..0])

drawGrid :: [[String]] -> String
drawGrid rr = intercalate "\n-+-+-\n" $ intercalate "|" <$> rr

-- | Takes a 1D line and tries to turn it into a 2D square.
--
-- If a square cannot be made, the rest goes immediately after.
toGrid :: [x] -> [[x]]
toGrid x = splitEvery (floor $ sqrt $ fromIntegral $ length x) x
    where
    splitEvery _ [] = []
    splitEvery n xx = pre : splitEvery n post
        where (pre, post) = splitAt n xx

drawPosition :: Board -> Position -> String
drawPosition b pos = case at pos b of
    Nothing -> grey $ show $ fromEnum pos + 1
    Just X  -> red  $ show X
    Just O  -> blue $ show O

grey, red, blue :: String -> String
grey s            = "\ESC[90m" <> s <> "\ESC[39m"
red  s            = "\ESC[31m" <> s <> "\ESC[39m"
blue s            = "\ESC[34m" <> s <> "\ESC[39m"

hideCursor, showCursor, enableAltDisplay, disableAltDisplay :: String
hideCursor        = "\ESC[?25l"
showCursor        = "\ESC[?25h"
enableAltDisplay  = "\ESC[?1049h"
disableAltDisplay = "\ESC[?1049l"

initScreen :: IO ()
initScreen = do
    hSetBuffering stdout $ BlockBuffering Nothing
    hSetBuffering stdin  NoBuffering
    hSetEcho stdin False
    putStr $ enableAltDisplay <> hideCursor

drawOutput :: State -> IO ()
drawOutput s = do
    putStr "\ESC[H\ESC[J"
    putStr $ draw s
    hFlush stdout

endScreen :: IO ()
endScreen = putStr $ disableAltDisplay <> showCursor
