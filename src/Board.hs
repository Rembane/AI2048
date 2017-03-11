module Board
  -- * Types!
  ( Board(..)
  , Direction(..)

  -- * Creation
  , createBoard
  , emptyBoard
  , fromList

  -- * Board manipulation functions
  , move
  , squish
  , updateCell

  -- * Board query functions
  , validMoves
  , emptyCells
  , hasWon

  -- * Utility functions
  , fancyBoard

  -- * Run function
  , runBoard
) where

import Control.Monad
import Control.Monad.Random.Lazy (Rand, evalRandIO, getRandomR, weighted)
import Data.List (intercalate, lookup)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import qualified Data.Vector as V
import qualified Data.Set as S
import System.Random

-- | Move direction
data Direction = UpD | RightD | DownD | LeftD
  deriving (Eq, Ord, Bounded, Enum, Show)

-- | A gameboard.
-- Nothing, when nothing in cell, Just Int when something in cell.
--
-- Coordinates:
-- (0,0) (0,1) (0,2) (0,3)
-- (1,0) (1,1) (1,2) (1,3)
-- (2,0) (2,1) (2,2) (2,3)
-- (3,0) (3,1) (3,2) (3,3)
newtype Board = Board { unpackBoard :: V.Vector (V.Vector (Maybe Int)) }
  deriving (Eq)

instance Show Board where
  show = fancyBoard

-- | Constructor
-- Adds to randomly added cells to the board.
createBoard :: (RandomGen g) => Rand g Board
createBoard = fmap fromJust . newCell . fromJust =<< newCell emptyBoard

emptyBoard :: Board
emptyBoard = (Board . V.replicate 4 . V.replicate 4) Nothing

-- | Create a board from a list of coordinates and values.
-- (row, col, value)
fromList :: [(Int, Int, Int)] -> Board
fromList = foldr (\(r,c,v) b -> updateCell (r,c) (Just v) b) emptyBoard

-- | Put either a 2 or a 4 in a random, empty cell.
newCell :: (RandomGen g) => Board -> Rand g (Maybe Board)
newCell b
  | null cs   = pure Nothing
  | otherwise = fmap Just b'

   where
     cs = emptyCells b

     b' = updateCell
            <$> ((cs V.!) <$> getRandomR (0, length cs - 1))
            <*> (Just <$> weighted [(4, 1%10), (2, 9%10)])
            <*> pure b

-- Update a cell at a certain coordinate.
updateCell :: (Int,Int) -> Maybe Int -> Board -> Board
updateCell (r,c) new (Board b) = let col = (b V.! r) V.// [(c, new)]
                                       in Board $ b V.// [(r, col)]

-- | Moves all the numbers in one direction and squishes numbers.
move :: (RandomGen g) => Direction -> Board -> Rand g (Maybe Board)
move d b = newCell $ squish d b

-- | Move all numbers to the right place.
squish :: Direction -> Board -> Board
squish d b = case d of
               UpD    -> (transposeBoard . mapOver id . transposeBoard) b
               RightD -> mapOver V.reverse b
               LeftD  -> mapOver id b
               DownD  -> (transposeBoard . mapOver V.reverse . transposeBoard) b
  where
    mapOver :: (V.Vector (Maybe Int) -> V.Vector (Maybe Int)) -> Board -> Board
    mapOver prep = Board . V.map (prep . addEmptyCells . squishRow . V.filter isJust . prep) . unpackBoard

    -- From left to right
    addEmptyCells v = v <> V.replicate (4 - length v) Nothing

    -- From left to right
    squishRow :: V.Vector (Maybe Int) -> V.Vector (Maybe Int)
    squishRow v = case length v of
                    0 -> mempty
                    1 -> v
                    _ -> let (v1, v2) = V.splitAt 2 v
                          in if v1 V.! 0 == v1 V.! 1
                                then (V.sum <$> sequenceA v1) `V.cons` squishRow v2
                                else v1 <> squishRow v2

-- | Transposes a board.
-- transposeBoard . transposeBoard == id
transposeBoard :: Board -> Board
transposeBoard (Board b) = Board $ V.fromList $ fmap (\c -> V.fromList $ fmap (\r -> (b V.! r) V.! c) [0..3]) [0..3]

-- | Draw the board in a nice way.
fancyBoard :: Board -> String
fancyBoard = intercalate "\n" . V.toList . V.map renderCols . unpackBoard
  where
    renderCols = intercalate "\t" . V.toList . V.map (maybe "" show)

-- | All the valid moves for this board.
validMoves :: Board -> S.Set Direction
validMoves b = (S.filter ((/= b) . (`squish` b)) . emptyDirections) b

-- | Have we won?
hasWon :: Board -> Bool
hasWon = (V.any . V.any) (== (Just 2048)) . unpackBoard

-- | The directions you get from empty cells.
emptyDirections :: Board -> S.Set Direction
emptyDirections = S.fromList . concat . V.toList . V.map go . emptyCells
  where
    go :: (Int, Int) -> [Direction]
    go (r,c) = catMaybes [ lookup r [(0, DownD),  (3, UpD)]
                         , lookup c [(0, RightD), (3, LeftD)]
                         ]

-- Return the indexes of all the cells that do not contain anything.
emptyCells :: Board -> (V.Vector (Int, Int))
emptyCells = join . V.imap (\ri r -> V.map (\ci -> (ri,ci)) $ V.findIndices isNothing r) . unpackBoard

runBoard :: Rand StdGen Board -> IO Board
runBoard b = evalRandIO b
