{- This module upholds the following laws:

[__'init': Specifies starting 'turn'.__]:
    ∀(p :: Player). turn (init p) = p

[__'init': Has no marked squares.__]:
    ∀(p :: Player) (pos :: Position). at pos (init p) = Nothing

[__'init': No one has won yet.__]:
    won init = Nothing


[__'turn': Changes each valid move.__]:
    ∀(pos :: Position) (b :: Board). play pos b ≠ b
    ⇒ turn (play pos b) ≠ turn b


[__'play': Equals 'id' if the 'Position' is already occupied or game is won.__]:
    ∀(pos :: Position) (b :: Board). at pos b ≠ Nothing ∨ won b ≠ Nothing
    ⇒ play pos b = b

[__'play' Determines 'at' when the 'Position' is not occupied and game is not won.__]:
    ∀(pos :: Position) (b :: Board). at pos b = Nothing ∧ won b = Nothing
    ⇒ at pos (play pos b) = Just (turn b)
-}
module Game.Internal where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.List (transpose)
import Data.Maybe

data Board = Board
    { turn'    :: Player
    -- | Must always be total (every position must be in this).
    , contents' :: Map Position (Maybe Player)
    } deriving (Eq, Ord, Show, Read)

data Position = TopLeft    | TopMiddle    | TopRight
              | MiddleLeft | Middle       | MiddleRight
              | BottomLeft | BottomMiddle | BottomRight
              deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Player = X | O deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- * Construction

init :: Player -- ^ Who goes first?
     -> Board
init p = Board
    { turn'    = p
    , contents' = Map.fromList [(pos, Nothing) | pos <- [minBound..maxBound]]
    }

-- * Query

turn :: Board -> Player
turn = turn'

contents :: Board -> Map Position (Maybe Player)
contents = contents'

at :: Position -> Board -> Maybe Player
at p b = contents' b Map.! p

won :: Board -> Maybe Player
-- Remember, both winnning is impossible, and therefore doesn't need to be handled.
won b = head' $ filter (\xx -> and
        [ length xx == 3
        , and $ zipWith (==) xx $ tail xx
        , all isJust xx
        ])
    $ fmap (flip at b . toEnum) <$> diagonals <> cols <> rows
    where
    head' ((x:_):_) = x
    head' _         = Nothing
    diagonals = [[0,4,8], [2,4,6]]
    cols      = transpose rows
    rows      = [[0,1,2], [3,4,5], [6,7,8]]

-- * Operations

-- | id if 'Position' is already occupied or game is won.
play :: Position -> Board -> Board
play p b
    | any isJust [won b, at p b] = b
    | otherwise = Board
        { turn'    = if turn b == X then O else X
        , contents' = Map.adjust (\_-> Just $ turn b) p $ contents b
        }
