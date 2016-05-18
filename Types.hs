module Types (
  -- * Types!
  Direction(..)

  -- * Stateful helper functions
  , randomRSt
  , randomDirSt

  -- * More utilities
  , roundSucc
) where

import Control.Monad.State
import System.Random

data Direction = UpD | RightD | DownD | LeftD
  deriving (Eq, Ord, Bounded, Enum, Show)

-- | Like succ but starts over at the first value when the last has been reached.
roundSucc :: (Eq b, Bounded b, Enum b) => b -> b
roundSucc x | maxBound == x = minBound
            | otherwise     = succ x

instance Random Direction where
  random g = let (r, g') = randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g
              in (toEnum r, g')

  randomR (a,b) g = let (r, g') = randomR (fromEnum a, fromEnum b) g
                     in (toEnum r, g')

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt = state . randomR

randomDirSt :: (RandomGen g) => State g Direction
randomDirSt = state $ random

