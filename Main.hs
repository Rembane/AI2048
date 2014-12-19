module Main where

import Control.Monad
import Control.Monad.State
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

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt (lo, hi) = state $ randomR (lo, hi)

randomDirSt :: (RandomGen g) => State g Direction
randomDirSt = state $ random 

fancyBoard :: Board -> String
fancyBoard b = "X\tX\tX\tX\n" ++ V.foldr (\row s' -> (V.foldr (\col s -> (maybe "" show col) ++ '\t':s) "" row) ++ '\n':s') "" b
    
updateCell :: (Int,Int) -> Int -> Board -> Board
updateCell (r,c) new b = let col = (b V.! r) V.// [(c, Just new)] in b V.// [(r, col)]

createBoard :: (RandomGen g) => State g Board
createBoard = fromJust $ liftM (\b -> join $ fmap (fromJust . newCell) b) $ newCell empty
    where 
        empty = V.replicate 4 (V.replicate 4 Nothing)

-- Return the indexes of all the cells that do not contain anything.
freeCells :: Board -> Maybe (V.Vector (Int, Int))
freeCells b | V.null li = Nothing
            | otherwise = Just li
    where
        li = join $ V.imap (\ri r -> V.map (\ci -> (ri,ci)) $ V.findIndices isNothing r) b

newCell :: (RandomGen g) => Board -> Maybe (State g Board)
newCell b = fmap (\ps -> (grawr ps) >>= (\(randomIdx, newValue) -> return $ updateCell (ps V.! randomIdx) newValue b))
                $ freeCells b
    where
        grawr :: (RandomGen g) => V.Vector (Int, Int) -> State g (Int, Int)
        grawr ps = do
            randomIdx <- randomRSt (0, (V.length ps) - 1) 
            newValue  <- (randomRSt (0, 9) >>= (\x -> return $ distribution x))

            return (randomIdx, newValue)

        distribution :: Int -> Int
        distribution i | i == 9    = 4
                       | otherwise = 2

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
              | otherwise    = maybeToList b' ++ (allBoards g $ fromJust b')
    where
        -- b' = fmap (\d1 -> join $ fmap (\d2 -> newCell $ move d2 b) d1) 
        b' = (Just randomDirSt) >>= (\d1 -> (d1 >>= (\d2 -> newCell $ move d2 b)))
        -- b' = fmap (\d1 -> d1 >>= (\d2 -> move d2 b)) 
        {-
        grawr = do
            dir <- randomDirSt

            newCell $ move dir b

        b' = fromJust $ grawr
        -}

        -- b' = fmap (\dir -> liftM (\x -> runState x g) (newCell $ move dir b)) randomDirSt
        -- b' = fmap (\bx -> runState bx g) (randomDirSt >>= (\dir -> (newCell . move) dir b))
        -- (b', g') = fmap (\b1 -> runState b1 g) (fmap (\dir -> newCell $ move dir b) randomDirSt) 


main = do
    g <- newStdGen
    let bs = allBoards g $ createBoard
    mapM_ (putStrLn . fancyBoard) bs 

