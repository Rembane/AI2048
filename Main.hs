module Main where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import System.Random

data Direction = DUp | DRight | DDown | DLeft
    deriving (Eq, Ord, Bounded, Enum, Show)

type Board = V.Vector (V.Vector (Maybe Int))

instance Random Direction where
    random g = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g of
                    (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                           (r, g') -> (toEnum r, g')

randomDirection :: (RandomGen g) => g -> (Direction, g)
randomDirection g = random g

fancyBoard :: Board -> String
fancyBoard b = "X\tX\tX\tX\n" ++ V.foldr (\row s' -> (V.foldr (\col s -> (maybe "" show col) ++ '\t':s) "" row) ++ '\n':s') "" b
    
updateCell :: (Int,Int) -> Int -> Board -> Board
updateCell (r,c) new b = let col = (b V.! r) V.// [(c, Just new)] in b V.// [(r, col)]

createBoard :: (RandomGen g) => g -> Board
createBoard g = fromJust $ (newCell g empty) >>= (newCell g)
    where 
        empty = V.replicate 4 (V.replicate 4 Nothing)

-- Return the indexes of all the cells that do not contain anything.
freeCells :: Board -> Maybe (V.Vector (Int, Int))
freeCells b | V.null li = Nothing
            | otherwise = Just li
    where
        li = join $ V.imap (\ri r -> V.map (\ci -> (ri,ci)) $ V.findIndices isNothing r) b

newCell :: (RandomGen g) => g -> Board -> Maybe Board
newCell g b = fmap (\poss -> updateCell ((V.!) poss $ fst $ randomR (0, (V.length poss) - 1) g) 2 b) (freeCells b)

squish :: [Int] -> [Int]
squish []       = []
squish (a:[])   = [a]
squish (a:b:xs) = if a == b 
                  then a+b:(squish xs)
                  else a:(squish (b:xs))

moveOneDimension :: Direction -> V.Vector (Maybe Int) -> V.Vector (Maybe Int)
moveOneDimension dir v = 
    case dir of 
        DLeft  -> sqshd V.++ onlyHoles
        DRight -> onlyHoles V.++ sqshd
    where 
        sqshd     = V.fromList $ map Just $ squish $ catMaybes $ V.toList v
        onlyHoles = V.replicate (V.length v - V.length sqshd) Nothing

-- Cols become rows. Like for matrices.
-- (transposeBoard . transposeBoard) == id
transposeBoard :: Board -> Board
transposeBoard b = V.map (\c -> V.map (\r -> (b V.! r) V.! c) rows) cols
    where 
        (rows, cols) = (V.fromList [0..(V.length b) - 1], V.fromList [0..(V.length $ b V.! 0) - 1])

move :: Direction -> Board -> Board
move DUp    v = transposeBoard $ V.map (moveOneDimension DLeft) $ transposeBoard v
move DDown  v = transposeBoard $ V.map (moveOneDimension DRight) $ transposeBoard v
move DLeft  v = V.map (moveOneDimension DLeft) v
move DRight v = V.map (moveOneDimension DRight) v

allBoards :: (RandomGen g) => g -> Board -> [Board]
allBoards g b | isNothing b' = []
              | otherwise    = maybeToList b' ++ (allBoards g' $ fromJust b')
    where
        b' = newCell g $ move m b
        (m, g') = randomDirection g

main = do
    g <- newStdGen
    let bs = allBoards g $ createBoard g
    mapM_ (putStrLn . fancyBoard) bs 

