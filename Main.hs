module Main where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
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


-- | Evaluate an AI.
evalAI :: State StdGen [Board] -> IO [Board]
evalAI b = do
  g <- newStdGen
  return $ evalState b g

main :: IO ()
main = mapM_ print =<< evalAI (rotatingAI UpD =<< createBoard)
