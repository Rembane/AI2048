module Main where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import System.Random

data Direction = DUp | DRight | DDown | DLeft
    deriving (Show, Eq)

type Board = V.Vector (V.Vector (Maybe Int))

pairify :: [Int] -> [(Int, Int)]
pairify (a:b:xs) = (a,b):pairify xs

updateCell :: (Int,Int) -> Int -> Board -> Board
updateCell (r,c) new b = let col = (b V.! r) V.// [(c, Just new)] in b V.// [(r, col)]

createBoard :: (RandomGen g) => g -> Board
createBoard g = updateCell (r2, c2) 2 $ updateCell (r1, c1) 2 empty
    where 
        empty               = V.replicate 4 (V.replicate 4 Nothing)
        ((r1,c1):(r2,c2):_) = take 2 $ nub $ pairify $ randomRs (0,3) g

newCell :: (RandomGen g) => g -> Board -> Board
newCell g b = updateCell (r,c) 2 b
    where
        ((r,c):_) = dropWhile (\(r1,c1) -> isJust $ (b V.! r1) V.! c1) $ nub $ pairify $ randomRs (0,3) g

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
transposeBoard :: Board -> Board
transposeBoard b = V.map (\c -> V.map (\r -> (b V.! r) V.! c) rows) cols
    where 
        (rows, cols) = (V.fromList [0..(V.length b) - 1], V.fromList [0..(V.length $ b V.! 0) - 1])

move :: Direction -> Board -> Board
move DUp    v = transposeBoard $ V.map (moveOneDimension DLeft) $ transposeBoard v
move DDown  v = transposeBoard $ V.map (moveOneDimension DRight) $ transposeBoard v
move DLeft  v = V.map (moveOneDimension DLeft) v
move DRight v = V.map (moveOneDimension DRight) v

main = do
    g <- newStdGen
    let b = createBoard g
    print b
    let b1 = newCell g $ move DRight b
    print b1
    let b2 = newCell g $ move DUp b1
    print b2

