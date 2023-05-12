module StringAlignment where

scoreMatch = 0

scoreMismatch = -1

scoreSpace = -1

string1 = "writers"
string2 = "vintner"

x = "aferociousmonadatemyhamster" 

y = "functionalprogrammingrules"

z = "bananrepubliksinvasionsarmestabsadjutant"
t = "kontrabasfiolfodralmakarmästarlärling"

-- a) Write a Haskell function that returns the score of the optimal alignment of the two strings string1 and string2. If you need to, consult the Hint section below.
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] string2 = (length string2) * scoreSpace
similarityScore string1 [] = (length string1) * scoreSpace
similarityScore string1 string2 = sim (string1, string2)

sim :: (String, String) -> Int
sim ((x : xs), (y : ys)) = maximum [((similarityScore xs ys) + score (x, y)), ((similarityScore xs (y : ys)) + score (x, '-')), ((similarityScore (x : xs) ys) + score ('-', y))]

score :: (Char, Char) -> Int
score (x, '-') = scoreSpace
score ('-', y) = scoreSpace
score (x, y)
  | x == y = scoreMatch
  | x /= y = scoreMismatch

newSimilarityScore :: String -> String -> Int
newSimilarityScore string1 string2 = newSim (length string1) (length string2)
  where
    newSim i j = mcsTable !! i !! j
    mcsTable = [[mcsEntry i j | j <- [0 ..]] | i <- [0 ..]]

    mcsEntry :: Int -> Int -> Int
    mcsEntry 0 0 = 0
    mcsEntry i 0 = (length string1) * scoreSpace
    mcsEntry 0 j = (length string2) * scoreSpace
    mcsEntry i j =
      maximum
        [ (newSim (i - 1) (j - 1)) + score (x, y),
          (newSim (i - 1) j) + score (x, '-'),
          (newSim i (j - 1)) + score ('-', y)
        ]
      where
        x = string1 !! (i - 1)
        y = string2 !! (j - 1)

--

-- b) Explain what the following Haskell function does.
-- It attaches the two arguments as heads on each list, i.e.
-- attachHeads 'a' 'b' [(['A'..'F'], ['H'..'K'])] returns [("aABCDEF","bHIJK")], where a and b are inserted first in the two lists.
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1 : xs, h2 : ys) | (xs, ys) <- aList]

-- Same as above, just appends as tails to the lists instead of head
attachTails :: a -> a -> [([a], [a])] -> [([a], [a])]
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs, ys) <- aList]

-- c) Write a Haskell function which generalizes the maximum function in two respects:

-- The "value" of an element is defined by a function supplied as a parameter.
-- Instead of just one element, the result is a list of all maximum elements.
-- For example, maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn [] = []
maximaBy valueFcn list = maximaByHelp valueFcn list big
  where
    big = maximum (map valueFcn list)

-- help function to store the biggest value troughout the iteration.
maximaByHelp _ [] _ = []
maximaByHelp valueFcn (x : xs) big
  | (valueFcn x) == big = [x] ++ maximaByHelp valueFcn xs big
  | otherwise = maximaByHelp valueFcn xs big

-- d.) Let
type AlignmentType = (String, String)

-- Write a Haskell function which returns a list of all optimal alignments between string1 and string2.
-- (Hint: Follow the same pattern as you did in part a., and make use of the functions defined in parts b. and c.)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
--optAlignments _ [] = [([], [])]
--optAlignments [] _ = [([], [])]
optAlignments string1 [] = [(string1, concat $ replicate (length string1) "-")]
optAlignments [] string2 = [(concat $ replicate (length string2) "-", string2)]
-- optAlignments (x : xs) [] = attachHeads x '-' (optAlignments xs [])
-- optAlignments [] (y : ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x : xs) (y : ys) = maximaBy (uncurry similarityScore) (concat [attachHeads x y (optAlignments xs ys), attachHeads x '-' (optAlignments xs (y : ys)), attachHeads '-' y (optAlignments (x : xs) ys)])

newOptAlignments :: String -> String -> (Int, [AlignmentType])
newOptAlignments string1 string2 = newOpt (length string1) (length string2)
  where
    newOpt :: Int -> Int -> (Int, [AlignmentType])
    newOpt i j = mcsTable !! i !! j
    mcsTable = [[mcsEntry i j | j <- [0 ..]] | i <- [0 ..]]

    mcsEntry :: Int -> Int -> (Int, [AlignmentType])
    mcsEntry 0 0 = (0, [([], [])])
    mcsEntry i 0 = (i * scoreSpace , [(string1, concat $ replicate (length string1) "-")])
    mcsEntry 0 j = (j * scoreSpace , [(concat $ replicate (length string2) "-", string2)])
    mcsEntry i j = (maximum $ map fst alignments, concatMap snd $ maximaBy fst alignments)
      --(1, bestAlignment)
      where
        --bestAlignment = maximaBy (uncurry newSimilarityScore) (concat $ map snd alignments)
        alignments = [(a + score (x,y) ,attachHeads x y as),(b + score (x,'-'), attachHeads x '-' bs), (c + score ('-',y), attachHeads '-' y cs)]
        (a, as) = newOpt (i - 1) (j - 1)
        (b, bs) = newOpt i (j - 1)
        (c, cs) = newOpt (i - 1) j
        x = string1 !! (i - 1)
        y = string2 !! (j - 1)

-- e.) Write a Haskell function
-- that prints all optimal alignments between string1 and string2 to the screen in a neat and easy-to-read fashion.
-- outputOptAlignments string1 string2
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  putStrLn (outputLine string1)
  putStrLn "\n"
  putStrLn (outputLine string2)

outputLine :: String -> String
outputLine [] = []
outputLine (x : xs)
  | xs /= [] = ([x] ++ " " ++ (outputLine xs)) -- toUpper?
  | xs == [] = [x]

mcsLength' :: Eq a => [a] -> [a] -> Int
mcsLength' _ [] = 0
mcsLength' [] _ = 0
mcsLength' (x : xs) (y : ys)
  | x == y = 1 + mcsLength' xs ys
  | otherwise =
      max
        (mcsLength' xs (y : ys))
        (mcsLength' (x : xs) ys)

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable !! i !! j
    mcsTable = [[mcsEntry i j | j <- [0 ..]] | i <- [0 ..]]

    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y = 1 + mcsLen (i - 1) (j - 1)
      | otherwise =
          max
            (mcsLen i (j - 1))
            (mcsLen (i - 1) j)
      where
        x = xs !! (i - 1)
        y = ys !! (j - 1)