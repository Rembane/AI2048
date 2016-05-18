module Types (
  -- * Types!
  Direction(..)

  -- * Stateful helper functions
  , randomRSt
  , randomDirSt
) where

import Control.Monad.State
import System.Random

data Direction = UpD | RightD | DownD | LeftD
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Random Direction where
  random g = let (r, g') = randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g
              in (toEnum r, g')

  randomR (a,b) g = let (r, g') = randomR (fromEnum a, fromEnum b) g
                     in (toEnum r, g')

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt = state . randomR

randomDirSt :: (RandomGen g) => State g Direction
randomDirSt = state $ random

