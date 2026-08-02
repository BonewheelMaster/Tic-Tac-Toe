module Bot where

import Game

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree

-- | The bot picks a move.
--
-- This is a full tree search (Tic-Tac-Toe's tree is small enough to do so).
decide :: Board -> Position
decide b = move
    where
    -- head can be used here because we are only considering boards with exactly
    -- one move to the original.
    move = fst $ fst $ head $ filter (\((_,x),(_,y)) -> x /= y)
        $ zip (Map.assocs $ contents b) (Map.assocs $ contents mostValued)

    mostValued = minimumBy (flip compare)
        -- We can ignore moves that help the opponent, because we are always
        -- guaranteed a win or tie move for us.
        [b | Node (b,p) _ <- bb, p /= Just other]

    -- Just can be used here because gameTree includes all boards.
    Just (Node _ bb) = treeLookup b $ label gameTree
    other            = if turn b == X then O else X

-- | The entire tree of Tic-Tac-Toe.
--
-- This is not a function, but a constant value. Therefore, GHC will memoize it
-- automatically.
gameTree :: Tree Board
gameTree = Node (Game.init X) [unfoldTree inner $ Game.init X, unfoldTree inner $ Game.init O]
    where
    inner b = if isNothing $ won b
        then (b, [play m b | m <- availMoves b])
        else (b, [])

-- | Label each node of the tree based on the best win outcome or tie the current
-- player can guarantee; in effect, the value of each board position.
--
-- Assumes the bot is O.
label :: Tree Board -> Tree (Board, Maybe Player)
label (Node b []) = case won b of
    Nothing -> Node (b, Nothing) []
    p       -> Node (b, p      ) []
label (Node b tt) = case turn b of
    -- X is min and O is max because the bot is O. This won't work if bot is X.
    X -> Node (b, minimumBy ord pp) tt'
    O -> Node (b, maximumBy ord pp) tt'
    where
    tt' = label <$> tt
    pp  = [p | Node (_,p) _ <- tt']

treeLookup :: (Eq x, Eq a) => x -> Tree (x,a) -> Maybe (Tree (x,a))
treeLookup x t@(Node (y,_) tt)
    | x == y    = Just t
    | otherwise = head' $ filter isJust $ treeLookup x <$> tt
    where
    head' []    = Nothing
    head' (h:_) = h

-- | X < Nothing < O
ord :: Maybe Player -> Maybe Player -> Ordering
x `ord` y
    | x == y           = EQ
    | all isJust [x,y] = compare x y
    | otherwise        = case (x,y) of
        (Just X,  Nothing) -> LT
        (Just O,  Nothing) -> GT
        (Nothing, Just X)  -> GT
        (Nothing, Just O)  -> LT
