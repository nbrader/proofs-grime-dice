#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck --package containers-0.6.5.1
import Test.QuickCheck
import Data.List (nub, sortBy)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Data.Ratio ((%))
import Control.Applicative
import Data.Ord (comparing)

type Prob s = s
type Val v = v

data PMF v s = PMF {pmfMap :: Map [v] s} deriving (Eq, Show)

-- Example dice definitions using `Map`
redDie :: PMF Rational Rational
redDie = PMF $ Map.fromList [([4], 5 % 6), ([9], 1 % 6)]

greenDie :: PMF Rational Rational
greenDie = PMF $ Map.fromList [([0], 1 % 6), ([5], 5 % 6)]

blueDie :: PMF Rational Rational
blueDie = PMF $ Map.fromList [([2], 3 % 6), ([7], 3 % 6)]

purpleDie :: PMF Rational Rational
purpleDie = PMF $ Map.fromList [([1], 2 % 6), ([6], 4 % 6)]

yellowDie :: PMF Rational Rational
yellowDie = PMF $ Map.fromList [([3], 4 % 6), ([8], 2 % 6)]

-- Example enumeration of dice colors
data DieColours = Red | Green | Blue | Purple | Yellow deriving (Eq, Show, Enum, Bounded)

allDieColours :: [DieColours]
allDieColours = [minBound .. maxBound]
allDicePMFs = [redDie, greenDie, blueDie, purpleDie, yellowDie]
allDice = zip allDieColours allDicePMFs

-- Function to verify probabilities sum to 1
validateDie :: (Eq s, Num s) => PMF v s -> Bool
validateDie die = sum (Map.elems $ pmfMap die) == 1

-- Function to calculate joint probability of two dice
getJoint :: (Ord v, Num s) => PMF v s -> PMF v s -> PMF v s
getJoint (PMF pmf1) (PMF pmf2) =
    PMF $ Map.fromListWith (+)
        [ (v1 ++ v2, p1 * p2) | 
            (v1, p1) <- Map.toList pmf1,
            (v2, p2) <- Map.toList pmf2
        ]

getJointFromList :: (Ord v, Num s) => [PMF v s] -> PMF v s
getJointFromList = foldr1 getJoint

-- Function to calculate PMF over (first > second, first < second, tie)
-- pmfTiedOrWon :: (Ord v, Num s) => [PMF v s] -> [Bool]
pmfTiedOrWon pmfs = [sum [if valueIsEqualToMaxPredicate !! i then prob else 0 | (valueIsEqualToMaxPredicate, prob) <- zip valueIsEqualToMaxListOfPredicates (Map.elems (pmfMap jointPMF))] | i <- diceIndices]
  where jointPMF = getJointFromList pmfs
        diceIndices = [0 .. (length pmfs - 1)]
        jointOutcomes = Map.keys (pmfMap jointPMF)
        maxes = map maximum jointOutcomes
        valueIsEqualToMaxListOfPredicates = zipWith (\maxVal jOutcomes -> map (== maxVal) jOutcomes) maxes jointOutcomes

test3PlayerGames = mapM_ print $ map realToFrac $ pmfTiedOrWon allDicePMFs



sectionTitle :: String -> IO ()
sectionTitle msg = do
    putStrLn $ getZipList (ZipList (repeat '-') <* ZipList msg)
    putStrLn msg
    putStrLn $ getZipList (ZipList (repeat '-') <* ZipList msg)

-- Test PMF over outcome comparisons for all pairs of dice
validateDice :: [(DieColours, PMF Rational Rational)] -> IO ()
validateDice dice = do
    sectionTitle "Validating dice..."
    forM_ allDice $ \(color, die) -> do
        let isValid = validateDie die
        putStrLn $ show color ++ ": " ++ if isValid then "Valid" else "Invalid"

-- Generate a TGF representation of the graph with labeled edges
generateTGF :: [(DieColours, PMF Rational Rational)] -> String
generateTGF dice =
    let nodes = nub $ map fst dice
        edges =
            [ (c1, c2, realToFrac firstTiedOrWonProb)
            | (c1, pmf1) <- dice,
              (c2, pmf2) <- dice,
              let tiedOrWonProbs = pmfTiedOrWon [pmf1, pmf2],
              let firstTiedOrWonProb = tiedOrWonProbs !! 0,
              firstTiedOrWonProb > 0.5  -- "more than half" condition
            ]
        nodeSection = unlines [show i ++ " " ++ show color | (i, color) <- zip [1 ..] nodes]
        edgeSection =
            unlines
                [ show (nodeIndex c1) ++ " " ++ show (nodeIndex c2) ++ " " ++ show firstTiedOrWonProb
                | (c1, c2, firstTiedOrWonProb) <- edges
                ]
        nodeIndex color = case lookup color (zip nodes [1 ..]) of
            Just idx -> idx
            Nothing -> error "Color not found in nodes"
     in nodeSection ++ "#\n" ++ edgeSection

-- Print the graph in TGF format
printTGF :: String -> IO ()
printTGF tgf = do
    sectionTitle "Trivial Graph Format (TGF) with Labeled Edges:"
    putStrLn tgf

-- Updated main to include TGF generation and printing
main :: IO ()
main = do
    let tgf = generateTGF allDice
    printTGF tgf
