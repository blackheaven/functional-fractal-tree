module FractalTree
    ( drawTree
    ) where

import Test.QuickCheck

data Pixel = Space | One deriving (Eq, Show)
instance Arbitrary Pixel where
    arbitrary = elements [Space, One]

newtype Row = Row [Pixel]

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

-- | generate the lines to be displayed
-- 
-- >>> drawTree 0 0
-- []
drawTree :: Int -> Int -> [String]
drawTree _ _ = map render []
