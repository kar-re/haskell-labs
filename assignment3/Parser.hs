module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-), newLine, dropComment) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

-- The parser m -# n accepts the same input as m # n, but returns just the result from the n parser. 
-- The function should be declared as a left associative infix operator with precedence 7. Example:
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd



(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

space :: Parser Char
space = char ? isSpace

-- spaces accepts any number of whitespace characters as defined by the Prelude function isSpace.
spaces :: Parser String
spaces = iter space

token :: Parser a -> Parser a
token m = m #- spaces

noEmpty :: Parser String 
noEmpty = (char ? isSpace) # iter (char ? isSpace) >-> cons

comment :: Parser String
comment = (chars 2 ? (== "--")) -# iter (char ? (/='\n')) #- require "\n"

removeCommentOrSpace :: Parser String
removeCommentOrSpace = iter (comment ! noEmpty) >-> concat

dropComment :: Parser String 
dropComment s = return (dropWhile (/= '\n') s) ""

-- letter is a parser for a letter as defined by the Prelude function isAlpha.
letter :: Parser Char
letter = char ? isAlpha
-- letter =  error "letter not implemented"

word :: Parser String
word = token (letter # iter letter >-> cons)

-- The parser chars n accepts n characters.
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = token (chars (length w)) ? (==w)

-- The parser require w accepts the same string input as accept w but reports the missing string using err in case of failure.
require :: String -> Parser String
require w = accept w ! err ("error " ++ w)
-- require w  = error "require not i mplemented"

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

isNotNewLine :: Char -> Bool
isNotNewLine c
    | c == '\n' = False
    | otherwise = True

{- newLine :: Parser Char
newLine = char ? isNotNewLine

comment :: Parser String
comment = iter newLine

comments :: String -> String
comments [] = []
comments (x:xs)
    | x == '\n' = x:xs
    | otherwise = comments xs -}

newLine :: Parser String
newLine = iter $ char ? isNotNewLine

