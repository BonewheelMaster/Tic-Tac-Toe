module Game
    ( Board
    , Position(..)
    , Player(..)

    -- * Construction

    , init

    -- * Query

    , turn
    , contents
    , availMoves
    , at
    , won
    , tie

    -- * Operations

    , play
    ) where

import Game.Internal

import Prelude hiding (init)
import Data.Maybe

tie :: Board -> Bool
tie b = isNothing (won b) && all (isJust . flip at b) [minBound..maxBound]

availMoves :: Board -> [Position]
availMoves b = filter (isNothing . flip at b) [minBound..maxBound]
