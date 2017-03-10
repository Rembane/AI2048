module Board (
  -- * Types!
  Board(..)
  , Direction

  -- * Constructor
  , createBoard

  -- * Board manipulation functions
  , move

  -- * Board query functions
  , validMoves
  , freeCells
  , hasWon

  -- * Utility functions
  , fancyBoard
  , transposeBoard
  , runBoard
) where

import Control.Monad
import Control.Monad.Random.Lazy (Rand, evalRandIO, getRandomR, weighted)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import qualified Data.Vector as V
import qualified Data.Set as S
import System.Random

-- | Move direction
data Direction = UpD | RightD | DownD | LeftD
  deriving (Eq, Ord, Bounded, Enum, Show)

-- Nothing, when nothing in cell, Just Int when something in cell.
newtype Board = Board (V.Vector (V.Vector (Maybe Int)))
  deriving (Eq)

instance Show Board where
  show = fancyBoard

-- | Constructor
createBoard :: (RandomGen g) => Rand g Board
createBoard = fmap fromJust . newCell . fromJust
            =<< (newCell . Board . V.replicate 4 . V.replicate 4) Nothing

-- | Put either a 2 or a 4 in a random, empty cell.
newCell :: (RandomGen g) => Board -> Rand g (Maybe Board)
newCell b
  | V.null cs = pure Nothing
  | otherwise = fmap Just b'

   where
     cs = freeCells b

     -- Update a cell at a certain coordinate.
     updateCell :: (Int,Int) -> Int -> Board -> Board
     updateCell (r,c) new (Board b) = let col = (b V.! r) V.// [(c, Just new)]
                                       in Board $ b V.// [(r, col)]

     b' = updateCell
            <$> ((cs V.!) <$> getRandomR (0, (V.length cs) - 1))
            <*> weighted [(4, 1%10), (2, 9%10)]
            <*> pure b

-- | Moves all the numbers in one direction and squishes numbers.
move :: (RandomGen g) => Direction -> Board -> Rand g (Maybe Board)
move d b = newCell $ squish d b

-- | Move all numbers to the right place.
squish :: Direction -> Board -> Board
squish d (Board b) = Board $ case d of
                               UpD    -> mapOverCols id b
                               RightD -> mapOverRows id b
                               LeftD  -> mapOverRows id b
                               DownD  -> mapOverCols id b
  where
    transposeVVs vvs = let (Board vvs') = transposeBoard (Board vvs) in vvs'

    mapOverCols prep = transposeVVs . mapOverRows prep . transposeVVs
    mapOverRows prep = V.map (addEmptyCells . squishRow . V.filter isJust . prep)

    addEmptyCells v = let len = V.length v
                          cs  = V.replicate (4 - len) Nothing
                       in case d of
                            UpD    -> cs V.++ v
                            RightD -> cs V.++ v
                            LeftD  -> v V.++ cs
                            DownD  -> v V.++ cs

    squishRow :: V.Vector (Maybe Int) -> V.Vector (Maybe Int)
    squishRow v = case V.length v of
                    0 -> V.empty
                    1 -> v
                    _ -> let (v1, v2) = V.splitAt 2 v
                          in if v1 V.! 0 == v1 V.! 1
                                then (V.sum <$> sequenceA v1) `V.cons` squishRow v2
                                else v1 <> squishRow v2

-- | Draw the board in a nice way.
fancyBoard :: Board -> String
fancyBoard (Board b) = "\t\t\t\n" ++ V.foldr (\row s' -> (V.foldr (\col s -> (maybe "" show col) ++ '\t':s) "" row) ++ '\n':s') "" b

-- | All the valid moves for this board.
validMoves :: Board -> S.Set Direction
validMoves (Board b) = emptyDirections (Board b)
  `S.union` V.foldl (\s r -> S.union s $ if rowCanMove r then S.fromList [LeftD, RightD] else S.empty) S.empty b
  `S.union` if V.or $ V.zipWith colCanMove b (V.tail b) then S.fromList [UpD, DownD] else S.empty

  where
    rowCanMove r = canSquish r (V.tail r)
    colCanMove   = canSquish
    -- Takes two rows. If two pairwise elements are equal we can move.
    canSquish r1 r2 = V.or $ V.zipWith (==) r1 r2

-- | Have we won?
hasWon :: Board -> Bool
hasWon (Board b) = (V.any . V.any) (== (Just 2048)) b

-- | The directions you get from empty cells.
emptyDirections :: Board -> S.Set Direction
emptyDirections (Board b) = rows b `S.union` cols b
  where
    if' :: Bool -> a -> a -> a
    if' True  a _ = a
    if' False _ b = b

    rows b = V.foldl (\s r -> S.union s $ S.fromList $
                       if (V.head r) == Nothing then [RightD] else [] ++
                       if (V.last r) == Nothing then [LeftD]  else [] ++
                       if (V.any (== Nothing) . V.init . V.tail) r then [RightD, LeftD] else []
                     ) S.empty b

    cols b = S.fromList $
      if V.any (== Nothing) (V.head b) then [DownD] else [] ++
      if V.any (== Nothing) (V.last b) then [UpD]   else [] ++
      if (V.any (V.any (== Nothing)) . (V.init . V.tail)) b then [DownD, UpD] else []

-- Return the indexes of all the cells that do not contain anything.
freeCells :: Board -> (V.Vector (Int, Int))
freeCells (Board b) = join $ V.imap (\ri r -> V.map (\ci -> (ri,ci)) $ V.findIndices isNothing r) b

-- | Transpose a board.
-- Cols become rows. Like for matrices.
-- (transposeBoard . transposeBoard) == id
transposeBoard :: Board -> Board
transposeBoard (Board b) = Board $ V.map (\c -> V.map (\r -> (b V.! r) V.! c) rows) cols
    where
        (rows, cols) = (V.fromList [0..(V.length b) - 1], V.fromList [0..(V.length $ b V.! 0) - 1])

-- | Translates a boardstate into a board we can show
runBoard :: Rand StdGen Board -> IO Board
runBoard b = evalRandIO b
