module Statement (T, parse, toString, fromString, exec) where

import Dictionary qualified
import Expr qualified
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

data Statement
  = Assignment String Expr.T
  | If Expr.T Statement Statement
  | Begin [Statement]
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  | Skip
  | Comment String
  deriving (Show)

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

_if = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf

begin = accept "begin" -# iter parse #- require "end" >-> Begin

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile

_read = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

skip = accept "skip" #- require ";" >-> buildSkip

comment = accept "--" -# dropComment #- require "\n" >-> Comment

buildAss (v, e) = Assignment v e

buildIf ((e, x), y) = If e x y

buildWhile (e, x) = While e x

buildSkip _ = Skip

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment string expr : stmts) dict input = exec stmts (Dictionary.insert (string, Expr.value expr dict) dict) input
exec (If cond thenStmts elseStmts : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input
exec (Begin s : stmts) dict input = exec (s ++ stmts) dict input
exec (While expr s : stmts) dict input =
  if Expr.value expr dict > 0
    then exec (s : While expr s : stmts) dict input
    else exec stmts dict input
exec (Read string : stmts) dict (input : inputs) = exec stmts (Dictionary.insert (string, input) dict) inputs
exec (Write expr : stmts) dict input = Expr.value expr dict : exec stmts dict input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Comment string : stmts) dict input = exec stmts dict input

indent n = replicate (2 * n) ' '

stringify :: Int -> Statement -> String
stringify n (Assignment string expr) = indent n ++ string ++ " := " ++ Expr.toString expr ++ ";\n"
stringify n (If cond thenStmts elseStmts) = indent n ++ "if " ++ Expr.toString cond ++ " then\n" ++ stringify (n + 1) thenStmts ++ indent n ++ "else\n" ++ stringify (n + 1) elseStmts
stringify n (Begin s) = indent n ++ "begin\n" ++ concatMap (stringify (n + 1)) s ++ indent n ++ "end"
stringify n (While expr s) = indent n ++ "while " ++ Expr.toString expr ++ " do\n" ++ stringify (n + 1) s ++ "\n"
stringify n (Read string) = indent n ++ "read " ++ string ++ ";\n"
stringify n (Write expr) = indent n ++ "write " ++ Expr.toString expr ++ ";\n"
stringify n (Skip) = indent n ++ "skip;\n"
stringify n (Comment string) = indent n ++ "--" ++ string ++ "\n"

instance Parse Statement where
  parse = assignment ! _if ! begin ! while ! _read ! write ! skip ! comment
  toString = stringify 0
