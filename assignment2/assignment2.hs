module StringAlignment where

-- a) Write a Haskell function that returns the score of the optimal alignment of the two strings string1 and string2. If you need to, consult the Hint section below.
similarityScore :: String -> String -> Int
similarityScore (x:xs) (y:ys)
    | (y == ' ') and (x != ' ') == similarityScore (x:xs) ('-':ys)
    | (x == ' ') and (y != ' ') == similarityScore ('-':xs) (y:ys)
    | otherwise = sim ((x:xs),(y:ys))

sim((x:xs),(y:ys)) = max {sim(xs,ys) + score(x,y),
                          sim(xs,(y:ys)) + score(x,'-'),
                          sim((x:xs),ys) + score('-',y)}

score(x,'-') = score('-',y) = scoreSpace
score(x,y) = scoreMatch, if x == y
             scoreMismatch, if x /= y

-- b) Explain what the following Haskell function does.
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1 : xs, h2 : ys) | (xs, ys) <- aList]

-- c) Write a Haskell function which generalizes the maximum function in two respects:

-- The "value" of an element is defined by a function supplied as a parameter.
-- Instead of just one element, the result is a list of all maximum elements.
-- For example, maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].
maximaBy :: Ord b => (a -> b) -> [a] -> [a]

maximaBy valueFcn xs

-- d.) Let
type AlignmentType = (String, String)

-- Write a Haskell function which returns a list of all optimal alignments between string1 and string2.
-- (Hint: Follow the same pattern as you did in part a., and make use of the functions defined in parts b. and c.)

optAlignments :: String -> String -> [AlignmentType]

optAlignments string1 string2

-- e.) Write a Haskell function
-- that prints all optimal alignments between string1 and string2 to the screen in a neat and easy-to-read fashion.
outputOptAlignments string1 string2