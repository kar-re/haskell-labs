module StringAlignment where

  scoreMatch = 0

  scoreMismatch = -1

  scoreSpace = -1

  string1 = "writers"

  string2 = "vintner"

    -- a) Write a Haskell function that returns the score of the optimal alignment of the two strings string1 and string2. If you need to, consult the Hint section below.
  similarityScore :: String -> String -> Int
  similarityScore [] [] = 0
  similarityScore [] string2 = (length string2) * scoreSpace
  similarityScore string1 [] = (length string1) * scoreSpace
  similarityScore string1 string2 = sim (string1,string2)

  sim :: (String, String) -> Int
  sim ((x:xs),(y:ys)) = maximum [((similarityScore xs ys) + score (x,y)),((similarityScore xs (y:ys)) + score(x,'-')),((similarityScore (x:xs) ys) + score('-',y))]

  score :: (Char,Char) -> Int
  score (x,'-') = scoreSpace
  score ('-',y) = scoreSpace
  score (x,y) 
    |x == y = scoreMatch
    |x /= y = scoreMismatch

  -- b) Explain what the following Haskell function does.
  -- It attaches the two arguments as heads on each list, i.e. 
  -- attachHeads 'a' 'b' [(['A'..'F'], ['H'..'K'])] returns [("aABCDEF","bHIJK")], where a and b are inserted first in the two lists.
  attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
  attachHeads h1 h2 aList = [(h1 : xs, h2 : ys) | (xs, ys) <- aList]



  -- c) Write a Haskell function which generalizes the maximum function in two respects:

  -- The "value" of an element is defined by a function supplied as a parameter.
  -- Instead of just one element, the result is a list of all maximum elements.
  -- For example, maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].
  maximaBy :: Ord b => (a -> b) -> [a] -> [a]
  maximaBy valueFcn [] = []
  maximaBy valueFcn list = maximaByHelp valueFcn list big
    where big = maximum (map valueFcn list)

  -- help function to store the biggest value troughout the iteration.
  maximaByHelp _ [] _ = []
  maximaByHelp valueFcn (x:xs) big
    | (valueFcn x) == big = [x] ++ maximaByHelp valueFcn xs big
    | otherwise =  maximaByHelp valueFcn xs big

  -- d.) Let
  type AlignmentType = (String, String)

  -- Write a Haskell function which returns a list of all optimal alignments between string1 and string2.
  -- (Hint: Follow the same pattern as you did in part a., and make use of the functions defined in parts b. and c.)

  optAlignments :: String -> String -> [AlignmentType]
  optAlignments [] [] = [([],[])]
  optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
  optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
  optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) (concat [attachHeads x y (optAlignments xs ys), attachHeads x '-' (optAlignments xs (y:ys)), attachHeads '-' y (optAlignments (x:xs) ys)])

  -- e.) Write a Haskell function
  -- that prints all optimal alignments between string1 and string2 to the screen in a neat and easy-to-read fashion.
  -- outputOptAlignments string1 string2
  outputOptAlignments :: String -> String -> IO()
  outputOptAlignments string1 string2 = do
    putStrLn (outputLine string1)
    putStrLn "\n"
    putStrLn (outputLine string2)

  outputLine :: String -> String
  outputLine [] = []
  outputLine (x:xs)
   |xs /= [] = ([x] ++ " " ++ (outputLine xs)) --toUpper?
   |xs == [] = [x]
