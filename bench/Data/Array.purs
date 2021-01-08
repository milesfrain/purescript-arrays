module Bench.Data.Array where

import Prelude

import Data.Array (concat, difference, intersect, length, mapMaybe, nubEq, range, replicate, reverse, slice, take, takeEnd, union, zipWith)
import Data.Foldable (sum)
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (BenchResult, benchWith', withUnits)

benchArray :: Effect Unit
benchArray = do
  let
    arr1 = arrayGen 500 100 [1,0,5,5]
    arr2 = arrayGen 0 10000 [1,2,0]

    onlyEven x = if x `mod` 2 == 0 then Just x else Nothing

  bench1 "mapMaybe" (mapMaybe onlyEven) arr1
  bench1 "mapMaybe" (mapMaybe onlyEven) arr2
  bench1 "nubEq" nubEq arr1
  bench1 "nubEq" nubEq arr2
  bench2 "union" union arr1 arr2
  bench2 "union" union arr2 arr1
  bench2 "intersect" intersect arr1 arr2
  bench2 "intersect" intersect arr2 arr1
  bench2 "difference" difference arr1 arr2
  bench2 "difference" difference arr2 arr1

-- | Benchmarking wrapper for a function that takes a single array
bench1 :: forall a. String -> (Array a -> Array a) -> Array a -> Effect Unit
bench1 label func arr = do
  log "---------------"
  log $ label <> " (" <> show (length arr) <> ") -> "
    <> (show $ length $ func arr)
  log "---------------"
  tunedBench \_ -> func arr

-- | Benchmarking wrapper for a function that takes two arrays
bench2 :: forall a. String -> (Array a -> Array a -> Array a) -> Array a -> Array a -> Effect Unit
bench2 label func arr1 arr2 = do
  log "---------------"
  log $ label <> " (" <> show (length arr1) <> ", " <> show (length arr2) <> ") -> "
    <> (show $ length $ func arr1 arr2)
  log "---------------"
  tunedBench \_ -> func arr1 arr2

-- Runs a maximum of 1000 trials or up to 1 second of benchmarks.
-- Could end up with much longer than 1 second if first trial is very slow.
-- Opportunities for improvement:
--   Use Aff to timeout slow benchmarks (e.g. more than 5 seconds).
--   Not throw away the tuning trials.
--   Keep running until trial or time target is reached.
tunedBench :: forall a. (Unit -> a) -> Effect Unit
tunedBench func = do
  -- Somewhat arbitrary choice of tuning trials,
  -- but good ignore the first cold-cache run.
  tune <- benchWith' 2 func
  let trials = min 1000 $ ceil $ 1.0e9 / tune.min
  res <- benchWith' trials func
  log $ showBenchResult res
  log $ "Trials: " <> show trials <> ", Total time: " <> withUnits (res.mean * toNumber trials)

-- This should be exposed by library
showBenchResult :: BenchResult -> String
showBenchResult res =
       "mean   = " <> withUnits res.mean
  <> "\nstddev = " <> withUnits res.stdDev
  <> "\nmin    = " <> withUnits res.min
  <> "\nmax    = " <> withUnits res.max

-- | Integer division where the result is rounded up.
-- | Intended for unsigned values
divUp :: Int -> Int -> Int
divUp n d = (n + d - 1) / d

-- | Generates an array of a desired size filled with
-- | duplicates or skipped values based on provided pattern.
-- |
-- | Example:
-- | arrayGen 5 10 [1,0,3]
-- |
-- | Initial values based on desired start:
-- | [5,6,  7  ,8,9,   10   ,11,12,  13   ]
-- | Duplicate counts created by repeating provided pattern:
-- | [1,0,  3  ,1,0,    3   ,1 ,0,    3   ]
-- | Initial values duplicated or dropped by pattern:
-- | [5,  7,7,7,8,  10,10,10,11,  13,13,13]
-- | Trimmed result to match output size:
-- | [5,  7,7,7,8,  10,10,10,11,  13]
-- |
-- | Also shuffles pattern assignment, and final ordering.
-- | For example, the above example could have three 5's
-- | scattered throughout the output array.
-- |
arrayGen :: Int -> Int -> Array Int -> Array Int
arrayGen start total pattern =
  let
    cycles = divUp total $ sum pattern
    initialSize = cycles * length pattern
    initial = range start $ start + initialSize - 1
    fullPattern = power pattern cycles

    shuffleInitial = shuffle initial

  in
    shuffle
      $ take total
      $ concat
      $ zipWith replicate fullPattern shuffleInitial

-- | Performs a structured (non-random) shuffle of array elements
-- | which should be good enough for benchmarking most code that
-- | involves sorting.
-- |
-- | Simulates a riffle shuffle where half the deck is flipped.
-- |
-- | Example:
-- | shuffle [1,2,3,4,5,6,7,8] == [1,8,2,7,3,6,4,5]
-- |
shuffle :: forall a. Array a -> Array a
shuffle arr =
  let
    len = length arr
    halfLen = len / 2
    firstHalf = take halfLen arr
    lastHalf = takeEnd halfLen arr
    -- Will be the center element for odd number of elements.
    -- Otherwise an empty array.
    oddPiece = slice halfLen ((len - 1) / 2) arr
  in
    append oddPiece
      $ concat
      $ zipWith (\a b -> [a,b]) firstHalf $ reverse lastHalf
