module GFO
  ( Param (..),
    Params (..),
    SolveSettings (..),
    SolveState (..),
    WithMomentum (..),
    step,
    stripMomentum,
    zeroMomentum,
  )
where

import Control.Monad.Identity (Identity (..), runIdentity)
import Control.Monad.State (State, runState, state)
import Data.Kind (Type)
import System.Random (RandomGen, UniformRange, uniform, uniformR)

class Param s a | a -> s where
  randomUnit :: RandomGen g => g -> (a, g)
  zero :: a
  scale :: s -> a -> a
  add :: a -> a -> a

class Params s (f :: (Type -> Type) -> Type) | f -> s where
  updateParams ::
    Applicative m =>
    (forall a. Param s a => x a -> m (y a)) ->
    f x ->
    m (f y)

data WithMomentum a = WithMomentum
  { wmParam :: !a,
    wmMomentum :: !a
  }

data SolveSettings p s = SolveSettings
  { ssExponent :: s,
    ssStepSizeGrow :: s,
    ssStepSizeShrink :: s,
    ssMomentumShrink :: s,
    ssErrorFn :: p Identity -> s
  }

data SolveState g p s = SolveState
  { ssRand :: g,
    ssParams :: p WithMomentum,
    ssError :: s,
    ssStepSize :: s
  }

zeroMomentum :: Params s p => p Identity -> p WithMomentum
zeroMomentum = runIdentity . updateParams (pure . flip WithMomentum zero . runIdentity)

stripMomentum :: Params s p => p WithMomentum -> p Identity
stripMomentum = runIdentity . updateParams (pure . Identity . wmParam)

mutate ::
  (RandomGen g, UniformRange s, Floating s, Params s p) =>
  s ->
  s ->
  p WithMomentum ->
  State g (p WithMomentum)
mutate expo stepSize = updateParams $ \WithMomentum {..} -> do
  sign <- state uniform
  randReal <- state (uniformR (0, 1))
  let s = (if sign then stepSize else -stepSize) * randReal ** expo
  u <- state randomUnit
  let m' = add wmMomentum (scale s u)
  pure $ WithMomentum (add wmParam m') m'

dampenMomentum ::
  Params s p =>
  s ->
  p WithMomentum ->
  p WithMomentum
dampenMomentum s =
  runIdentity
    . updateParams
      ( \(WithMomentum p m) ->
          pure (WithMomentum p (scale s m))
      )

step ::
  (UniformRange s, Floating s, Ord s, Params s p, RandomGen g) =>
  SolveSettings p s ->
  SolveState g p s ->
  SolveState g p s
step SolveSettings {..} ss@SolveState {..}
  | error' < ssError =
      ss
        { ssError = error',
          ssParams = ps',
          ssRand = g',
          ssStepSize = ssStepSize * ssStepSizeGrow
        }
  | otherwise =
      ss
        { ssParams = dampenMomentum ssMomentumShrink ssParams,
          ssRand = g',
          ssStepSize = ssStepSizeShrink * ssStepSize
        }
  where
    (ps', g') = runState (mutate ssExponent ssStepSize ssParams) ssRand
    error' = ssErrorFn (stripMomentum ps')

instance Param Double Double where
  randomUnit g =
    let (b, g') = uniform g
     in (if b then 1 else (-1), g')
  zero = 0
  scale = (*)
  add = (+)
