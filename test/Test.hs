module Main where

import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isNothing)
import qualified Data.Vector as V

import Board

main :: IO ()
main = hspec $ do
  describe "Board behaviours" $ do
    -- TODO: Rewrite as QuickCheck property.
    it "Puts the empty cells in the right places when the board is squished" $ do
      b <- runBoard createBoard
      let bs = map (\d -> squish d b) [UpD, RightD, DownD, LeftD]
          accessors =
            [ (\b c -> map (\r -> getCell (r,c) b) [3,2,1,0]) -- Up
            , (\b r -> map (\c -> getCell (r,c) b) [0,1,2,3]) -- Right
            , (\b c -> map (\r -> getCell (r,c) b) [0,1,2,3]) -- Down
            , (\b r -> map (\c -> getCell (r,c) b) [3,2,1,0]) -- Left
            ]
      and (zipWith checkForEmptyBoxes accessors bs) `shouldBe` True

getCell :: (Int, Int) -> Board -> Maybe Int
getCell (r,c) (Board b) = (b V.! r) V.! c

checkForEmptyBoxes :: (Board -> Int -> [Maybe Int]) -> Board -> Bool
checkForEmptyBoxes f b = all (notElem Nothing . dropWhile isNothing . f b) [0..3]

