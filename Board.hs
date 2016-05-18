module Board (
  -- * Types!
  Board(..)

  -- * Constructor
  , createBoard

  -- * Board manipulation functions
  , move

  -- * Board question functions
  , validMoves
  , freeCells

  -- * Utility functions
  , fancyBoard
  , transposeBoard
  , runBoard
) where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Set as S
import System.Random

import Types

-- Nothing, when nothing in cell, Just Int when something in cell.
newtype Board = Board (V.Vector (V.Vector (Maybe Int)))
  deriving (Eq)

instance Show Board where
  show = fancyBoard

-- | Constructor
createBoard :: (RandomGen g) => State g Board
createBoard = do
  Just b1 <- newCell $ Board $ V.replicate 4 (V.replicate 4 Nothing)
  Just b2 <- newCell b1
  return b2

-- | Put a "random" number in a random, empty cell.
newCell :: (RandomGen g) => Board -> State g (Maybe Board)
newCell b =
  let cs = freeCells b
   in if V.null cs
         then return Nothing
         else do
           value <- newValue
           pos   <- (cs V.!) <$> randomRSt (0, (V.length cs) - 1)
           return $ Just $ updateCell pos value b

-- | Creates a random cell value, either 2 or 4.
newValue :: (RandomGen g) => State g Int
newValue = (\i -> if i == 9 then 4 else 2) <$> randomRSt (0, 9 :: Int)

-- Update a cell at a certain coordinate.
updateCell :: (Int,Int) -> Int -> Board -> Board
updateCell (r,c) new (Board b) = let col = (b V.! r) V.// [(c, Just new)] in Board $ b V.// [(r, col)]

-- | Moves all the numbers in one direction and squishes numbers.
move :: (RandomGen g) => Direction -> Board -> State g (Maybe Board)
move d b = newCell $ squish d b

-- | Move all numbers to the right place.
squish :: Direction -> Board -> Board
squish d (Board b) = Board $ case d of
                       UpD    -> mapOverCols b
                       RightD -> mapOverRows b
                       LeftD  -> mapOverRows b
                       DownD  -> mapOverCols b
  where
    transposeVVs vvs = let (Board vvs') = transposeBoard (Board vvs) in vvs'
    mapOverCols = transposeVVs . mapOverRows . transposeVVs
    mapOverRows = V.map (addEmptyCells . squishVector)

    addEmptyCells v = let len = V.length v
                          cs  = V.replicate (4 - len) Nothing
                       in case d of
                            UpD    -> cs V.++ v
                            RightD -> cs V.++ v
                            LeftD  -> v V.++ cs
                            DownD  -> v V.++ cs

    squishVector = V.fromList . map Just . squishList . catMaybes . V.toList

    squishList :: [Int] -> [Int]
    squishList []       = []
    squishList (a:[])   = [a]
    squishList (a:b:xs) = if a == b
                             then a+b:(squishList xs)
                             else a:(squishList (b:xs))

-- | Draw the board in a nice way.
fancyBoard :: Board -> String
fancyBoard (Board b) = "X\tX\tX\tX\n" ++ V.foldr (\row s' -> (V.foldr (\col s -> (maybe "" show col) ++ '\t':s) "" row) ++ '\n':s') "" b

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
runBoard :: State StdGen Board -> IO Board
runBoard b = do
  g <- newStdGen
  return $ evalState b g
