module FractalTree
    ( drawTree
    ) where

import Test.QuickCheck
import Data.Monoid
import Data.List(group)

data Pixel = Space | One deriving (Eq, Show)
instance Arbitrary Pixel where
    arbitrary = elements [Space, One]

newtype Row = Row { unRow :: [Pixel] }

-- |
-- 
-- prop> length xs == length (render $ Row xs)
-- prop> length (filter (`notElem` ['_', '1'])  (render $ Row xs)) == 0
-- prop> length (filter (== Space) xs) == length (filter (== '_')  (render $ Row xs))
-- prop> length (filter (== One) xs) == length (filter (== '1')  (render $ Row xs))
render :: Row -> String
render (Row xs) = map convert xs
  where convert x = case x of
                      Space -> '_'
                      One   -> '1'

-- | Split the drawing space horizontally in the middle and apply each
-- function on each part
-- 
-- prop> x >= 0 ==> length (splitSpace x (`replicate` 'T') (`replicate` 'B')) == x
-- prop> x >= 0 ==> length (group (splitSpace x (`replicate` 'T') (`replicate` 'B'))) <= 2
-- prop> x >= 2 ==> map head (group (splitSpace x (`replicate` 'T') (`replicate` 'B'))) == ['T', 'B']
-- prop> x >= 2 ==> let [a,b] = map length (group (splitSpace x (`replicate` 'T') (`replicate` 'B'))) in elem (b-a) [0,1]
splitSpace :: Monoid a => Int -> (Int -> a) -> (Int -> a) -> a
splitSpace s t b = t (div s 2) <> b (div (s+1) 2)

-- |
-- 
-- prop> x > 0 && y > 0 ==> length (drawVerticalPartOfY x y) == y
-- prop> x > 0 && y > 0 ==> all (\r -> length r == x) (map unRow (drawVerticalPartOfY x y))
-- prop> x > 2 && y > 0 ==> all (\r -> let [a, b, c] = map length (group r) in b == 1 && elem (a-c) [0,1]) (map unRow (drawVerticalPartOfY x y))
drawVerticalPartOfY :: Int -> Int -> [Row]
drawVerticalPartOfY c r = replicate r $ Row $ replicate (div c 2) Space <> [One] <> replicate ((div (c+1) 2) - 1) Space

-- | generate the lines to be displayed
-- 
-- >>> drawTree 0 0
-- []
drawTree :: Int -> Int -> [String]
drawTree _ _ = map render []
