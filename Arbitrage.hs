#! /usr/bin/env nix-shell
#! nix-shell -p "haskell.packages.ghc865.ghcWithPackages (pkgs: [pkgs.containers pkgs.random-fu])" -i "cabal exec -- runghc"
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Monad (join)
import Data.List (nub, sortOn)
import qualified Data.Map as Map
import qualified Data.Ord
import Data.Ratio (Rational, (%))
import Data.Random (sample, randomElement)

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
  deriving (Show)

-- randomBets :: Int -> Int -> [(HorseBet,

extractChars :: HorseBet -> [Char]
extractChars (Win c) = [c]
extractChars (Place c) = [c]
extractChars (Exacta c1 c2) = [c1, c2]
extractChars (Quinella c1 c2) = [c1, c2]
extractChars (Lay bet) = extractChars bet

extractAllChars :: [HorseBet] -> [Char]
extractAllChars bets = nub . join $ extractChars <$> bets

allResults :: [Char] -> [RaceResult]
allResults chars =
  [ RaceResult {firstPlace, secondPlace}
  | firstPlace <- chars
  , secondPlace <- chars
  , firstPlace /= secondPlace
  ]

allResultsN :: Int -> [RaceResult]
allResultsN n = allResults $ take n ['a' ..]

allBetResults :: [HorseBet] -> [RaceResult]
allBetResults bets =
  case extractAllChars bets of
    [] -> allResultsN 2
    ['a'] -> allResults ['a', 'b']
    [x] -> allResults ['a', x]
    chars -> allResults chars

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

type Categorical = Map.Map RaceResult Rational

pr :: Categorical -> HorseBet -> Rational
pr dist bet = sum [coeff | (res, coeff) <- Map.toList dist, res |= bet]

partialDeriv :: [(HorseBet, Rational)] -> RaceResult -> Categorical -> Rational
partialDeriv bets raceResult dist =
  2 * sum [v - pr dist x | (x, v) <- bets, raceResult |= x]

gradient :: [(HorseBet, Rational)] -> [RaceResult] -> Categorical -> Categorical
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
     [(HorseBet, Rational)]
  -> [RaceResult]
  -> Categorical
  -> Rational
  -> Categorical
step bets raceResults dist stepSize =
  let delta = gradient bets raceResults dist
      updated = Map.unionWith (+) dist (Map.map (stepSize *) delta)
   in normalize updated

initialDist :: [RaceResult] -> Categorical
initialDist raceResults =
  Map.fromList
    [(res, 1 % fromIntegral (length raceResults)) | res <- raceResults]

regressionOrbit :: [(HorseBet, Rational)] -> [Categorical]
regressionOrbit bets =
  let raceResults = allBetResults (fst <$> bets)
      startModel = initialDist raceResults
   in startModel : scanl (step bets raceResults) startModel ((1 %) <$> [1 ..])

distance dist1 dist2 = let
  keys = nub $ Map.keys dist1 ++ Map.keys dist2
  get = Map.findWithDefault 0
  square x = x * x
  in sum [ square $ get k dist1 - get k dist2 | k <- keys ]

squaredError :: [(HorseBet, Rational)] -> Categorical -> Rational
squaredError bets dist = sum [square (v - pr dist p) | (p, v) <- bets]
  where
    square x = x * x

regressUntilEpsilon :: [(HorseBet, Rational)] -> Rational -> Categorical
regressUntilEpsilon bets epsilon =
  let orbit = drop 5 $ regressionOrbit bets
      -- err = squaredError bets
   in snd .
      head .
      -- dropWhile (\(dist1, dist2) -> abs (err dist1 - err dist2) > epsilon) $
      dropWhile (\(dist1, dist2) -> distance dist1 dist2 > epsilon) $
      zip orbit (tail orbit)

main :: IO ()
main =
  let bets = [(Win 'a', 1 % 3), (Lay $ Win 'a', 1 % 7)]
      epsilon = 1 % 10000
      model = regressUntilEpsilon bets epsilon
   in print $ [(bet, pr model bet) | (bet, _) <- bets]

-- Local Variables:
-- eval: (projectile-mode)
-- projectile-project-compilation-cmd: "./Arbitrage.hs"
-- End:
