module Utilities where
-- Takes two functions and creates a tuple wth f1 to x1 and f2 to x2?
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Applies function to value or something
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Returns just a or else nothing
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Takes a function that creates a maybe a and an a and returns an a 
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

