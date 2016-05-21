module Main where

import Control.Monad
import Control.Monad.State
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import System.Random

import Board
import Types

-- | This AI rotates the direction with one step for every step.
rotatingAI :: (RandomGen g) => Direction -> Board -> State g [Board]
rotatingAI d b = do
  b' <- move d b
  case b' of
    Nothing  -> return []
    Just b'' -> if hasWon b''
                   then return [b'']
                   else (b'':) <$> rotatingAI (roundSucc d) b''


-- | This AI makes the move that leads to the most number of free squares.
-- It can see into the future.
mostFreeAI :: (RandomGen g) => Board -> State g [Board]
mostFreeAI b = do
  moves <- fmap catMaybes $ mapM (\d -> ((fmap . fmap) (\b' -> (d, V.length $ freeCells b')) (move d b))) $ S.toList $ validMoves b
  case moves of
    [] -> return []
    xs -> do
      b' <- move (fst $ maximumBy (compare `on` snd) xs) b
      case b' of
        Nothing  -> return []
        Just b'' -> if hasWon b''
                       then return [b'']
                       else (b'':) <$> mostFreeAI b''

-- | Evaluate an AI.
evalAI :: State StdGen [Board] -> IO [Board]
evalAI b = do
  g <- newStdGen
  return $ evalState b g

main :: IO ()
main = mapM_ print =<< evalAI (mostFreeAI =<< createBoard)

-- main = mapM_ print =<< evalAI (rotatingAI UpD =<< createBoard)
