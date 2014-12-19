import Control.Monad.State
import System.Random

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt (lo, hi) = state $ randomR (lo, hi)

main = do
    g <- newStdGen
    print $ runState (randomRSt (0,9)) g
