#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck
import Test.QuickCheck
import Data.List (nub)
import Control.Monad

-- Define data types for both magmas
data M1 = A | B | C | D deriving (Eq, Show, Enum, Bounded)
data M2 = P | Q | R | S deriving (Eq, Show, Enum, Bounded)

-- First magma operation (non-associative)
magmaOp1 :: M1 -> M1 -> M1
magmaOp1 A A = B
magmaOp1 A B = A
magmaOp1 A C = C
magmaOp1 A D = D
magmaOp1 B A = A
magmaOp1 B B = C
magmaOp1 B C = D
magmaOp1 B D = B
magmaOp1 C A = C
magmaOp1 C B = D
magmaOp1 C C = B
magmaOp1 C D = A
magmaOp1 D A = D
magmaOp1 D B = C
magmaOp1 D C = A
magmaOp1 D D = B

-- Second magma operation (associative - Klein four-group)
magmaOp2 :: M2 -> M2 -> M2
magmaOp2 P P = P
magmaOp2 P Q = Q
magmaOp2 P R = R
magmaOp2 P S = S
magmaOp2 Q P = Q
magmaOp2 Q Q = P
magmaOp2 Q R = S
magmaOp2 Q S = R
magmaOp2 R P = R
magmaOp2 R Q = S
magmaOp2 R R = P
magmaOp2 R S = Q
magmaOp2 S P = S
magmaOp2 S Q = R
magmaOp2 S R = Q
magmaOp2 S S = P

-- Generic functions that work with either magma
class (Eq a, Show a) => MagmaElement a where
    carrier :: [a]
    op :: a -> a -> a

instance MagmaElement M1 where
    carrier = [A .. D]
    op = magmaOp1

instance MagmaElement M2 where
    carrier = [P .. S]
    op = magmaOp2

-- Check if middle associativity holds for a specific middle element
isMiddleAssoc :: MagmaElement a => a -> Bool
isMiddleAssoc m = and [op (op a m) b == op a (op m b) | 
                      a <- carrier, b <- carrier]

-- Helper function for folding with the magma operation
foldMagma :: MagmaElement a => a -> [a] -> a
foldMagma = foldl op

-- Check if fold-left-combine middle associativity holds
isFoldLeftCombineMiddleAssoc :: MagmaElement a => a -> Bool
isFoldLeftCombineMiddleAssoc m = and [
    let result1 = foldMagma x xs
        result2 = foldMagma y ys
        combined1 = op result1 result2
        combined2 = foldMagma x (xs ++ [m] ++ ys)
    in combined1 == combined2 |
    x <- carrier,
    y <- carrier,
    xs <- sequence [carrier],
    ys <- sequence [carrier]
    ]

-- Check if an element is a generator
isGeneratorM1 :: M1 -> Bool
isGeneratorM1 m = length (generateElements m) == length (carrier :: [M1])
  where
    generateElements x = nub $ concat $ take 4 $ iterate genStep [x]
    genStep elems = [op a b | a <- elems, b <- carrier]

isGeneratorM2 :: M2 -> Bool
isGeneratorM2 m = length (generateElements m) == length (carrier :: [M2])
  where
    generateElements x = nub $ concat $ take 4 $ iterate genStep [x]
    genStep elems = [op a b | a <- elems, b <- carrier]

-- Properties for both magmas
class (MagmaElement a, Arbitrary a) => TestMagma a where
    testMagma :: String -> a -> IO ()

instance TestMagma M1 where
    testMagma name _ = testMagmaGenericM1 name (carrier :: [M1])

instance TestMagma M2 where
    testMagma name _ = testMagmaGenericM2 name (carrier :: [M2])

testMagmaGenericM1 :: String -> [M1] -> IO ()
testMagmaGenericM1 name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorM1 m then "" else "not ") ++ "a generator") elems
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++ " does " ++ 
           (if isMiddleAssoc m then "" else "not ") ++ 
           "satisfy middle associativity") elems
    
    putStrLn "\nTesting which elements satisfy fold-left-combine middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++ " does " ++ 
           (if isFoldLeftCombineMiddleAssoc m then "" else "not ") ++ 
           "satisfy fold-left-combine middle associativity") elems

testMagmaGenericM2 :: String -> [M2] -> IO ()
testMagmaGenericM2 name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorM2 m then "" else "not ") ++ "a generator") elems
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++ " does " ++ 
           (if isMiddleAssoc m then "" else "not ") ++ 
           "satisfy middle associativity") elems
    
    putStrLn "\nTesting which elements satisfy fold-left-combine middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++ " does " ++ 
           (if isFoldLeftCombineMiddleAssoc m then "" else "not ") ++ 
           "satisfy fold-left-combine middle associativity") elems

-- Arbitrary instances
instance Arbitrary M1 where
    arbitrary = elements [A .. D]

instance Arbitrary M2 where
    arbitrary = elements [P .. S]

-- Main function to test both magmas
main :: IO ()
main = do
    testMagma "First Magma (Non-associative)" (undefined :: M1)
    testMagma "Second Magma (Klein four-group)" (undefined :: M2)