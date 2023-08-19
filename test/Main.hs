module Main (main) where

import Control.Monad (forM_)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..), runIdentity)
import GFO
import System.Random (mkStdGen)

newtype PolyParams f = PolyParams [f Double]

instance Params Double PolyParams where
  updateParams f (PolyParams xs) = PolyParams <$> traverse f xs

polyValue :: PolyParams Identity -> Double -> Double
polyValue (PolyParams xs) x =
  sum $
    zipWith (\k p -> k * x ** p) (runIdentity <$> xs) [0 ..]

polyError :: PolyParams Identity -> PolyParams Identity -> Double
polyError xs ys = sum $ do
  x <- [(-2) .. 2]
  [abs $ polyValue xs x - polyValue ys x]

main :: IO ()
main = do
  let testParams = PolyParams $ Identity <$> [3, 2, 1]
  let zeroParams = PolyParams $ Identity <$> replicate 3 0
  let settings =
        SolveSettings
          { ssExponent = 1,
            ssStepSizeGrow = 1.1,
            ssStepSizeShrink = 0.9,
            ssMomentumShrink = 0.5,
            ssErrorFn = polyError testParams
          }
  let state0 =
        SolveState
          { ssRand = mkStdGen 0,
            ssParams = zeroMomentum zeroParams,
            ssError = polyError testParams zeroParams,
            ssStepSize = 1
          }

  let steps = iterate (step settings) state0
  forM_ (take 50 steps) $ \SolveState {..} -> do
    let ps = case ssParams of PolyParams xs -> wmParam <$> xs
    let ms = case ssParams of PolyParams xs -> wmMomentum <$> xs
    putStrLn $ fold [show ssStepSize, "\t", show ssError, "\t", show ps]
    print $ show ms
