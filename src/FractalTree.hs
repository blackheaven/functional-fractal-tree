module FractalTree
    ( drawTree
    ) where

data Pixel = Space | One
newtype Row = Row [Pixel]

-- | generate the lines to be displayed
-- 
-- >>> drawTree 0 0
-- []
drawTree :: Int -> Int -> [String]
drawTree _ _ = []
