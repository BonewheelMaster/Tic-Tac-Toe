module UI.State
    ( State

    -- * Construction

    , startSetup

    -- * Query

    , isSetup
    , isPlaying
    , isEnded

    -- ** Playing

    , board

    -- ** Ended

    , lastBoard
    , winner

    -- * Operations

    -- ** Setup

    , submitSetup

    -- ** Playing

    , submitPlay

    -- * Record Fields
    --
    -- | These can be used like ordinary record fields, to get and set.

    -- ** Setup

    , inputBot
    , inputFirstPlayer

    -- ** Playing

    , bot
    ) where

import UI.State.Internal
