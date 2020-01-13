#! /usr/bin/env nix-shell
#! nix-shell -p "haskell.packages.ghc865.ghcWithPackages (pkgs: [pkgs.containers pkgs.random-fu pkgs.random pkgs.aeson pkgs.utf8-string])" -i "cabal exec -- runghc"
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad (join, replicateM)
import qualified Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Foldable (for_)
import Data.List (nub, sortOn)
import qualified Data.Map as Map
import qualified Data.Ord
import Data.Random (randomElement, sample)
import Data.Ratio ((%))
import GHC.Generics (Generic)
import System.IO (IOMode(WriteMode), hPutStr, withFile)
import System.Random (randomIO)
import Data.Char (toUpper)
import Text.Printf (printf)

data RaceResult = RaceResult
  { firstPlace :: Char
  , secondPlace :: Char
  } deriving (Eq, Ord, Show)

data HorseBet
  = Win Char
  | Place Char
  | Exacta Char
           Char
  | Quinella Char
             Char
  | Lay HorseBet
  deriving (Show, Generic, Eq, Ord)

instance Data.Aeson.ToJSON HorseBet

formatBet bet =
  toUpper <$>
  case bet of
   Win x -> "W/" ++ [x]
   Place x -> "P/" ++ [x]
   Exacta x y -> "Ex/" ++ [x,'-',y]
   Quinella x y -> "Q/" ++ [x,'-',y]
   Lay bet -> "Lay/" ++ formatBet bet

randomDistinctPair :: Eq a => IO a -> IO (a, a)
randomDistinctPair ma = do
  a <- ma
  a' <- ma
  if a == a'
    then randomDistinctPair ma
    else pure (a, a')

randomBet :: [Char] -> IO HorseBet
randomBet horses = do
  let randomHorse = sample $ randomElement horses
  randomBaseBet <-
    (sample . randomElement) =<<
    sequence
      [ Win <$> randomHorse
      , Place <$> randomHorse
      , uncurry Exacta <$> randomDistinctPair randomHorse
      , uncurry Quinella <$> randomDistinctPair randomHorse
      ]
  sample $ randomElement [randomBaseBet, Lay randomBaseBet]

randomBets :: Int -> Int -> IO [(HorseBet, Double)]
randomBets numberOfHorses numberOfBets =
  let horses = take numberOfHorses ['a' ..]
   in replicateM numberOfBets ((,) <$> (randomBet horses) <*> randomIO)

allResults :: [Char] -> [RaceResult]
allResults chars =
  [ RaceResult {firstPlace, secondPlace}
  | firstPlace <- chars
  , secondPlace <- chars
  , firstPlace /= secondPlace
  ]

allResultsN :: Int -> [RaceResult]
allResultsN n = allResults $ take n ['a' ..]

(|=) :: RaceResult -> HorseBet -> Bool
RaceResult {firstPlace} |= (Win horse) = firstPlace == horse
RaceResult {firstPlace, secondPlace} |= (Place horse) =
  firstPlace == horse || secondPlace == horse
RaceResult {firstPlace, secondPlace} |= (Exacta horse1 horse2) =
  firstPlace == horse1 && secondPlace == horse2
RaceResult {firstPlace, secondPlace} |= (Quinella horse1 horse2) =
  (firstPlace == horse1 && secondPlace == horse2) ||
  (firstPlace == horse2 && secondPlace == horse2)
raceResult |= (Lay bet) = not $ raceResult |= bet

type Categorical = Map.Map RaceResult Double

pr :: Categorical -> HorseBet -> Double
pr dist bet = sum [coeff | (res, coeff) <- Map.toList dist, res |= bet]

partialDeriv :: [(HorseBet, Double)] -> RaceResult -> Categorical -> Double
partialDeriv bets raceResult dist =
  2 * sum [y - pr dist p | (p, y) <- bets, raceResult |= p]

gradient :: [(HorseBet, Double)] -> [RaceResult] -> Categorical -> Categorical
gradient bets raceResults dist =
  Map.fromList [(res, partialDeriv bets res dist) | res <- raceResults]

-- Project onto the probability simplex
-- https://arxiv.org/pdf/1309.1541.pdf
normalize :: Categorical -> Categorical
normalize dist =
  let us = sortOn Data.Ord.Down $ Map.elems dist
      rho =
        maximum
          [ j
          | (u, j) <- zip us [1 ..]
          , u + (1 / fromIntegral j) * (1 - sum (take j us)) > 0
          ]
      lambda = 1 / fromIntegral rho * (1 - sum (take rho us))
   in Map.map (\v -> max (v + lambda) 0) dist

step ::
     [(HorseBet, Double)]
  -> [RaceResult]
  -> Categorical
  -> Double
  -> Categorical
step bets raceResults dist stepSize =
  let delta = gradient bets raceResults dist
      updated = Map.unionWith (+) dist (Map.map (stepSize *) delta)
   in normalize updated

initialDist :: [RaceResult] -> Categorical
initialDist raceResults =
  Map.fromList
    [ (res, fromRational $ 1 % fromIntegral (length raceResults))
    | res <- raceResults
    ]

regressionOrbit :: Int -> [(HorseBet, Double)] -> [Categorical]
regressionOrbit numberOfHorses bets =
  let raceResults = allResultsN numberOfHorses
      startModel = initialDist raceResults
   in startModel :
      scanl (step bets raceResults) startModel (fromRational . (1 %) <$> [1 ..])

-- square :: Num a => a -> a
-- square a = a * a
-- optimalStep bets raceResults dist =
--   let delta = gradient bets raceResults dist
--       stepSize =
--         sum [(y - pr dist p) * pr delta p | (p, y) <- bets] / 2 *
--         sum [square (pr delta p) | (p, y) <- bets]
--       updated = Map.unionWith (+) dist (Map.map (stepSize *) delta)
--    in normalize updated
-- regressionOrbit' :: Int -> [(HorseBet, Double)] -> [Categorical]
-- regressionOrbit' numberOfHorses bets =
--   let raceResults = allResultsN numberOfHorses
--       startModel = initialDist raceResults
--    in startModel :
--       iterate (optimalStep bets raceResults) startModel
distance dist1 dist2 =
  let keys = nub $ Map.keys dist1 ++ Map.keys dist2
      get = Map.findWithDefault 0
      square x = x * x
   in sum [square $ get k dist1 - get k dist2 | k <- keys]

regressUntilEpsilon :: Int -> [(HorseBet, Double)] -> Double -> Categorical
regressUntilEpsilon numberOfHorses bets epsilon =
  let orbit = drop 5 $ regressionOrbit numberOfHorses bets
   in snd . head . dropWhile (\(dist1, dist2) -> distance dist1 dist2 > epsilon) $
      zip orbit (tail orbit)

data BetRegressionSim = BetRegressionSim
  { initialBets :: [(HorseBet, Double)]
  , regressedBets :: [(HorseBet, Double)]
  } deriving (Show, Generic)

instance Data.Aeson.ToJSON BetRegressionSim

data BetRegressionSummary = BetRegressionSummary
  { inputBets :: Map.Map String Double
  , betOrbits :: Map.Map String [Double]
  } deriving (Show, Generic)

instance Data.Aeson.ToJSON BetRegressionSummary

runBetRegressionSim numberOfHorses numberOfBets = do
  bets <- randomBets numberOfHorses numberOfBets
  let epsilon = fromRational $ 1 % 100
      raceResults = allResultsN numberOfHorses
      model = regressUntilEpsilon numberOfHorses bets epsilon
  pure $
    BetRegressionSim
      { initialBets = bets
      , regressedBets = [(bet, pr model bet) | (bet, _) <- bets]
      }

saveRegressionSim numberOfHorses numberOfBets fileName = do
  sim <- runBetRegressionSim numberOfHorses numberOfBets
  putStrLn $ "Saving " ++ fileName
  withFile fileName WriteMode $ \handle ->
    hPutStr handle . BLU.toString $ Data.Aeson.encode sim


main :: IO ()
main = do
  let numberOfHorses, numberOfBets :: Int
      numberOfHorses = 5
      numberOfBets = 10
      epsilon :: Double = fromRational $ 1 % 1000000
  bets <- randomBets numberOfHorses numberOfBets
  let fullRawOrbit = regressionOrbit numberOfHorses bets
      rawOrbit = drop 5 $ fullRawOrbit
      fullOrbit :: [Map.Map RaceResult Double]
      fullOrbit = take 5 fullRawOrbit ++
        (fmap fst .
        takeWhile (\(dist1, dist2) -> distance dist1 dist2 > epsilon) $
        zip rawOrbit (tail rawOrbit))
      betOrbits = Map.fromList $ nub [(formatBet bet, [pr dist bet | dist <- fullOrbit]) | (bet, _) <- bets]
      betRegressionSummary = BetRegressionSummary (Map.mapKeys formatBet $ Map.fromList bets) betOrbits
      fileName = printf "orbit_%d_%d.json" numberOfHorses numberOfBets
  withFile fileName WriteMode $ \handle ->
    hPutStr handle . BLU.toString $ Data.Aeson.encode betRegressionSummary
-- Local Variables:
-- eval: (projectile-mode)
-- projectile-project-compilation-cmd: "./ArbitrageFloatOrbit.hs"
-- End:
