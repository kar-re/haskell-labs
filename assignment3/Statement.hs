module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Skip |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
_if = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
begin = accept "begin" -# iter parse #- require "end" >-> Begin
while = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
_read = accept "read" -# word #- require ";" >-> Read
write = accept "write" -# Expr.parse #- require ";" >-> Write
skip = accept "skip" # require ";" >-> buildSkip
comment = accept "--" -# newLine #- require "\n" >-> Comment 

buildAss (v, e) = Assignment v e
buildIf ((e,x), y) = If e x y
buildWhile (e, x) = While e x
buildSkip (v, x) = Skip

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment string expr: stmts) dict input = exec stmts (Dictionary.insert (string, Expr.value expr dict) dict) input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Begin s: stmts) dict input = exec (s ++ stmts) dict input
exec (While expr s: stmts) dict input =
    if (Expr.value expr dict)>0
    then exec (s:(While expr s):stmts) dict input
    else exec stmts dict input
exec (Read string: stmts) dict (input:inputs) = exec stmts (Dictionary.insert (string, input) dict) inputs 
exec (Write expr: stmts) dict input = (Expr.value expr dict): exec stmts dict input
exec (Skip: stmts) dict input = exec stmts dict input
exec (Comment string: stmts) dict input = exec stmts dict input

stringify :: Statement -> String
stringify (Assignment string expr) = string ++ ":=" ++ Expr.toString expr ++ ";'\n'"
stringify (If cond thenStmts elseStmts) = "if " ++ Expr.toString cond ++ "\nthen " ++ (stringify thenStmts) ++ "\nelse " ++ (stringify elseStmts) ++ "\n" 
stringify (Begin s) = "begin\n" ++ concatMap stringify s ++ "end\n"
stringify (While expr s) = "while " ++ Expr.toString expr ++ " do\n" ++ stringify s
stringify (Read string)  = "read " ++ string ++ ";\n"
stringify (Write expr) = "wrote " ++ Expr.toString expr ++ ";\n"
stringify (Skip) = "skip;\n"
stringify (Comment string) = "--" ++ string ++ "\n"

instance Parse Statement where
  parse = assignment ! _if ! begin ! while ! _read ! write ! skip ! comment
  toString = stringify
