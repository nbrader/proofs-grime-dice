#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck --package containers-0.6.5.1
import Test.QuickCheck
import Data.List (nub)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Data.Ratio ((%))
import Control.Applicative

type Prob s = s
type Val v = v

data Die v s = Die {diePMF :: Map [v] s} deriving (Eq, Show)

-- Example dice definitions using `Map`
redDie :: Die Rational Rational
redDie = Die $ Map.fromList [([4], 5 % 6), ([9], 1 % 6)]

greenDie :: Die Rational Rational
greenDie = Die $ Map.fromList [([0], 1 % 6), ([5], 5 % 6)]

blueDie :: Die Rational Rational
blueDie = Die $ Map.fromList [([2], 3 % 6), ([7], 3 % 6)]

purpleDie :: Die Rational Rational
purpleDie = Die $ Map.fromList [([1], 2 % 6), ([6], 4 % 6)]

yellowDie :: Die Rational Rational
yellowDie = Die $ Map.fromList [([3], 4 % 6), ([8], 2 % 6)]

-- Example enumeration of dice colors
data DieColours = Red | Green | Blue | Purple | Yellow deriving (Eq, Show, Enum, Bounded)

allDieColours :: [DieColours]
allDieColours = [minBound .. maxBound]
allDice = zip allDieColours [redDie, greenDie, blueDie, purpleDie, yellowDie]

-- Enumeration for outcomes of dice comparison
data Outcome = FirstGreater | SecondGreater | Tie deriving (Eq, Show, Ord)

-- Function to verify probabilities sum to 1
validateDie :: (Eq s, Num s) => Die v s -> Bool
validateDie die = sum (Map.elems $ diePMF die) == 1

-- Function to calculate joint probability of two dice
jointProbability :: (Ord v, Num s) => Die v s -> Die v s -> Die v s
jointProbability (Die pmf1) (Die pmf2) =
    Die $ Map.fromListWith (+)
        [ (v1 ++ v2, p1 * p2) | 
            (v1, p1) <- Map.toList pmf1,
            (v2, p2) <- Map.toList pmf2
        ]

-- Function to calculate PMF over (first > second, first < second, tie)
pmfOutcomeComparison :: (Ord v, Num s) => Die v s -> Die v s -> Map Outcome s
pmfOutcomeComparison (Die pmf1) (Die pmf2) =
    Map.fromListWith (+)
        [ (outcome, p1 * p2)
        | (v1, p1) <- Map.toList pmf1,
          (v2, p2) <- Map.toList pmf2,
          let outcome = case compare (head v1) (head v2) of
                            GT -> FirstGreater
                            LT -> SecondGreater
                            EQ -> Tie
        ]

sectionTitle :: String -> IO ()
sectionTitle msg = do
    putStrLn $ getZipList (ZipList (repeat '-') <* ZipList msg)
    putStrLn msg
    putStrLn $ getZipList (ZipList (repeat '-') <* ZipList msg)

-- Test PMF over outcome comparisons for all pairs of dice
validateDice :: [(DieColours, Die Rational Rational)] -> IO ()
validateDice dice = do
    sectionTitle "Validating dice..."
    forM_ allDice $ \(color, die) -> do
        let isValid = validateDie die
        putStrLn $ show color ++ ": " ++ if isValid then "Valid" else "Invalid"

-- Test PMF over outcome comparisons for all pairs of dice
testAllPairsOutcomeComparison :: [(DieColours, Die Rational Rational)] -> IO ()
testAllPairsOutcomeComparison dice = do
    sectionTitle "Testing outcome comparison for all pairs:"
    forM_ [(c1, d1, c2, d2) | (c1, d1) <- dice, (c2, d2) <- dice] $ \(c1, d1, c2, d2) -> do
        let pmfComparison = pmfOutcomeComparison d1 d2
        putStrLn $ "PMF over outcomes (" ++ show c1 ++ " vs " ++ show c2 ++ "):"
        print pmfComparison

-- Main function to test validation and joint probabilities
main :: IO ()
main = do
    validateDice allDice
    putStrLn ""
    testAllPairsOutcomeComparison allDice
