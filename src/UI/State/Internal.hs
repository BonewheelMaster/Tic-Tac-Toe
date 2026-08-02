{-# LANGUAGE MultiWayIf #-}
-- | This module ensures that State is never invalid, eg., it will never be
-- Playing if the game is already won.
module UI.State.Internal where

import Bot
import Game

import Prelude hiding (init)

data State = Setup
        -- | Does the player request a bot?
        { inputBot         :: Maybe Bool
        -- | The player wants X or O to go first
        , inputFirstPlayer :: Maybe Player
        }
    | Playing
        -- | True if playing the bot, False if two-player.
        { bot    :: Bool
        , board' :: Board
        }
    | Ended
        { lastBoard' :: Board
        , winner'    :: Maybe Player
        }
    deriving (Eq, Ord, Show, Read)

-- * Construction

startSetup :: State
startSetup = Setup Nothing Nothing

-- * Query

isSetup :: State -> Bool
isSetup (Setup _ _) = True
isSetup _           = False

isPlaying :: State -> Bool
isPlaying (Playing _ _) = True
isPlaying _             = False

isEnded :: State -> Bool
isEnded (Ended _ _) = True
isEnded _           = False

-- | Current board state
board :: State -> Board
board = board'

-- | Ending board state
lastBoard :: State -> Board
lastBoard = lastBoard'

-- | The winner, or Nothing if tied
winner :: State -> Maybe Player
winner = winner'

-- * Operations

-- | Given all the fields of Setup are not Nothing, continue to the next state.
--
-- This is id if any of the fields of Setup are Nothing, or the given state
-- is not Setup.
submitSetup :: State -> State
submitSetup (Setup (Just b) (Just p)) = runBot $ Playing b $ init p
submitSetup s = s

-- | Submit a position to play.
--
-- This is id if the State is not Playing, or the position is invalid.
submitPlay :: Position -> State -> State
submitPlay pos s
    | isPlaying s = runBot if
        | Just p <- won b' -> Ended b' $ Just p
        | tie b'           -> Ended b' Nothing
        | otherwise        -> s { board' = b' }
    | otherwise   = s
    where
    b' = play pos b
    b  = board' s

-- | Run the bot.
--
-- This is id if the state is not Playing, the turn is not O, or the bot is disabled.
runBot :: State -> State
runBot s
    | isPlaying s = if bot s && turn b == O
        then submitPlay (decide b) s
        else s
    | otherwise = s
    where b = board s
