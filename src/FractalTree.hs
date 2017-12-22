module FractalTree
    ( renderTree
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
splitSpace 0 _ _ = mempty
splitSpace s t b = t (div s 2) <> b (div (s+1) 2)

-- |
-- 
-- prop> x > 0 && y > 0 ==> length (drawVerticalPartOfY x y) == y
-- prop> x > 0 && y > 0 ==> all (\r -> length r == x) (map unRow (drawVerticalPartOfY x y))
-- prop> x > 2 && y > 0 && x > y ==> all (\r -> let [a, b, c] = map length (group r) in b == 1 && elem (a-c) [0,1]) (map unRow (drawVerticalPartOfY x y))
drawVerticalPartOfY :: Int -> Int -> [Row]
drawVerticalPartOfY c r = replicate r $ mkRowWithOneAtPosition c (div c 2)

-- |
-- 
-- prop> x > 0 && y >= 0 && x > y ==> length (unRow (mkRowWithOneAtPosition x y)) == x
-- prop> x > 0 && y >= 0 && x > y ==> length (filter (== One) (unRow (mkRowWithOneAtPosition x y))) == 1
-- prop> x > 0 && y >= 0 && x > y ==> let [a, b, c] = map length (group ([Space] ++ unRow (mkRowWithOneAtPosition x y) ++ [Space])) in b == 1 && a == y + 1
mkRowWithOneAtPosition :: Int -> Int -> Row
mkRowWithOneAtPosition c p = Row $ replicate p Space <> [One] <> replicate (c - p - 1) Space

-- |
-- 
-- prop> x > 0 && y > 0 && x >= y ==> length (drawLeftObliquePartOfY x y) == y
-- prop> x > 1 && y > 0 && x >= y ==> all (\r -> length r == x) (map unRow (drawLeftObliquePartOfY x y))
-- prop> x > 0 && y > 0 && x >= y ==> all (\r -> length (filter (== One) r) == 1) (map unRow (drawLeftObliquePartOfY x y))
-- prop> x > 0 && y > 0 && x >= y ==>  (\xs -> and (zipWith (\a b -> a == b - 1) xs (tail xs))) (map (length . takeWhile (/= One) . unRow) (drawLeftObliquePartOfY x y))
drawLeftObliquePartOfY :: Int -> Int -> [Row]
drawLeftObliquePartOfY b r = [ mkRowWithOneAtPosition b x | x <- [(b-r)..(b-1)], x >= 0 ]

-- |
-- 
-- prop> y > 1 ==> length (drawObliquePartsOfY ((2 * y) + 1) y) == y
-- prop> x > 1 && y > 0 && x >= y ==> all (\r -> length r == ((x*2) + 1)) (map unRow (drawObliquePartsOfY ((x*2) + 1) y))
-- prop> x > 1 && y > 0 && x >= y ==> all (\r -> length (filter (== One) r) == 2) (map unRow (drawObliquePartsOfY ((x*2) + 1) y))
-- prop> x > 1 && y > 0 && x >= y ==>  (\xs -> and (zipWith (\a b -> a == b - 1) xs (tail xs))) (map (length . takeWhile (/= One) . unRow) (drawObliquePartsOfY ((x*2) + 1) y))
-- prop> x > 1 && y > 0 && x >= y ==>  (\xs -> and (zipWith (\a b -> a == b - 1) xs (tail xs))) (map (length . takeWhile (/= One) . reverse . unRow) (drawObliquePartsOfY ((x*2) + 1) y))
-- prop> x > 1 && y > 0 && x >= y ==>  (\xs -> and (zipWith (\a b -> a == b + 2) xs (tail xs))) (map (length . dropWhile (/= One) . reverse . dropWhile (/= One) . unRow) (drawObliquePartsOfY ((x*2) + 1) y))
-- prop> x > 1 && y > 0 && x >= y ==>  all (>= 3) (map (length . dropWhile (/= One) . reverse . dropWhile (/= One) . unRow) (drawObliquePartsOfY ((x*2) + 1) y))
drawObliquePartsOfY :: Int -> Int -> [Row]
drawObliquePartsOfY b r = zipWith (\l r -> Row (take b (l <> [Space] <> r))) left (map reverse left)
  where left = map unRow (drawLeftObliquePartOfY (div b 2) r)

-- |
-- 
-- prop> y > 1 ==> length (drawY ((2 * y) + 1) y) == y
-- prop> x > 1 && y > 0 && x >= y ==> all (\r -> length r == ((x*2) + 1)) (map unRow (drawY ((x*2) + 1) y))
drawY :: Int -> Int -> [Row]
drawY c r = splitSpace r (drawObliquePartsOfY c) (drawVerticalPartOfY c)

-- |
-- 
-- prop> y > 1 ==> length (drawTree ((2 * y) + 1) y) == y
-- prop> x > 1 && y > 0 && x >= y ==> all (\r -> length r == ((x*2) + 1)) (map unRow (drawTree ((x*2) + 1) y))
drawTree :: Int -> Int -> [Row]
drawTree c r = splitSpace r (\h -> map Row (zipWith (<>) (left h) (right h))) (drawY c)
  where left h = map unRow (drawTree (div c 2) h)
        right h = map unRow (drawTree (div c 2 + mod c 2) h)

-- | generate the lines to be displayed
-- 
-- >>> renderTree 0 0
-- []
renderTree :: Int -> Int -> [String]
renderTree c r = map render (drawTree c r)
