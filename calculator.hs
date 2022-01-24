module Main where

import Data.Char
import Data.List

type Vars = String

-- arithmetic expressions from previous lectures
data AExpr = Var Vars | Const Integer
           | Add AExpr AExpr | Mul AExpr AExpr | Sub AExpr AExpr
     deriving (Eq,Show)

-- the binary operators
data BOps = AddOp | MulOp | SubOp deriving (Show,Eq)

-- the type of tokens
data Token = VSym Vars | BOp BOps | CSym Integer
           | LPar | RPar
           | PA AExpr
           | Err String
  deriving (Show,Eq)

-- Free variable function
fv :: AExpr -> [Vars]
fv (Var x) = [x]
fv (Const n) = []
fv (Add e1 e2) = nub $ fv e1 ++ fv e2
fv (Sub e1 e2) = nub $ fv e1 ++ fv e2
fv (Mul e1 e2) = nub $ fv e1 ++ fv e2

-- Values and Environments
type Value = Integer
type Env = [(Vars,Value)]

-- Evaluation function
eval :: Env -> AExpr -> Integer
eval e (Var x) = case lookup x e of
                  Just v -> v
                  Nothing -> error $ "No value for variable " ++ x
eval e (Const n) = n
eval e (Add e1 e2) = eval e e1 + eval e e2
eval e (Sub e1 e2) = eval e e1 - eval e e2
eval e (Mul e1 e2) = eval e e1 * eval e e2

-- let's make our own pretty printer
instance Show AExpr where
  -- show :: AExpr -> String
  show (Var x) = x
  show (Const n) = show n
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

{- Want:
 (2*x-3*y)*(z+1) :: String
   -->  (analyzer)
 [ LPar, CSym 2, BOp *, VSym "x", BOp -, ...]
   :: [Token]
   -->  (parser)
 Mul (Sub (Mul (Const 2) (Var "x")) (Mul (Const 3) (Var "y")))
     (Add (Var "z") (Const 1))
   :: AExpr -}


-- The LEXER
{-
EXPECTED BEHAVIOR:
lexer "(2*x-3*y)*(z+2)"
 = [ LPar, CSym 2, BOp MulOp, VSym "x", BOp SubOp, CSym 3, BOp MulOp, VSym "y",
     RPar, BOp MulOp, LPar, VSym "z", BOp AddOp, CSym 2, RPar]
-}


-- Helper functions that determine if a given string is a constant or a variable

-- A constant must begin with a digit, and be followed by zero or more digits.
isCSym :: String -> Bool
isCSym = let q0 "" = False
             q0 (x:xs) = isDigit x && q1 xs
             q1 "" = True
             q1 (x:xs) = isDigit x && q1 xs
          in q0

-- A variable must begin with a lowercase letter,
-- and be followed by zero or more letters, numbers, _, or '
isVSym :: String -> Bool
isVSym = let q0 ""     = False
             q0 (x:xs) | x `elem` ['a'..'z'] = q1 xs
                       | otherwise = False
             q1 [] = True
             q1 (x:xs) = (isLetter x || isDigit x || elem x "_'") && q1 xs
          in q0

-- Since we import Data.Char, the following functions are no longer needed
--
-- isDigit :: Char -> Bool
-- isDigit x = '0' <= x && x <= '9'
--
-- isLetter :: Char -> Bool
-- isLetter x = ('a' <= x &&  x <= 'z') || ('A' <= x && x <= 'Z')

{-
Approach no.1:
start with a preprocessor that adds spaces
Then, using Haskell's built-in "words" function,
obtain a list of individual tokens as strings
-}

preproc :: String -> String
preproc "" = ""
preproc ('(':xs) = ' ' : '(' : ' ' : preproc xs
preproc (')':xs) = ' ' : ')' : ' ' : preproc xs
preproc ('*':xs) = ' ' : '*' : ' ' : preproc xs
preproc ('+':xs) = ' ' : '+' : ' ' : preproc xs
preproc ('-':xs) = ' ' : '-' : ' ' : preproc xs
preproc (x:xs) = x : preproc xs

-- Here the analyze function will take a list of strings produced by "words"
analyze :: [String] -> [Token]
analyze [] = []
analyze ("(":xs) = LPar : analyze xs
analyze (")":xs) = RPar : analyze xs
analyze ("*":xs) = BOp MulOp : analyze xs
analyze ("+":xs) = BOp AddOp : analyze xs
analyze ("-":xs) = BOp SubOp : analyze xs
analyze (x:xs) | isCSym x = CSym (read x) : analyze xs
analyze (x:xs) | isVSym x = VSym x : analyze xs
analyze s = error $ "Token error: " ++ concat s

lexer1 :: String -> [Token]
-- lexer x = analyze (words (preproc x))
lexer1 = analyze . words . preproc

{-
Approach no.2
Start with a helper function that searches for the longest prefix of a list that
makes the given predicate True, and breaks the list into two parts
when the predicate becomes False.
-}

spanPrefix :: ([a] -> Bool) -> [a] -> ([a],[a])
spanPrefix p s0 = helper p [] s0     -- p is a predicate on *prefixes* of s
  where helper p s [] = (s,[])
        helper p s (x:xs) | p (s ++ [x]) = helper p (s ++ [x]) xs
                          | otherwise = (s,(x:xs))

-- Now the lexer goes directly letter-by-letter, searching for the longest
-- prefix in case of a variable or a constant
lexer2 :: String -> [Token]
lexer2 "" = []
lexer2 ('(':xs) = LPar : lexer2 xs
lexer2 (')':xs) = RPar : lexer2 xs
lexer2 ('*':xs) = BOp MulOp : lexer2 xs
lexer2 ('+':xs) = BOp AddOp : lexer2 xs
lexer2 ('-':xs) = BOp SubOp : lexer2 xs
lexer2 (x:xs) | isLower x = VSym        v : lexer2 r where (v,r) = spanPrefix isVSym (x:xs)
lexer2 (x:xs) | isDigit x = CSym (read c) : lexer2 r where (c,r) = spanPrefix isCSym (x:xs)
lexer2 s = [Err s]

-- Approach no.3: inline/adapt DFAs for each lexical category directly into the
-- analyzing function
lexer3 :: String -> [Token]
lexer3 "" = []
lexer3 ('(':xs) = LPar : lexer3 xs
lexer3 (')':xs) = RPar : lexer3 xs
lexer3 ('*':xs) = BOp MulOp : lexer3 xs
lexer3 ('+':xs) = BOp AddOp : lexer3 xs
lexer3 ('-':xs) = BOp SubOp : lexer3 xs
lexer3 (x:xs) | isDigit x = scanCSym [x] xs
  where scanCSym i "" = [CSym (read i)]
        scanCSym i (x:xs) | isLetter x = scanCSym (i ++ [x]) xs
                          | otherwise = CSym (read i) : lexer3 (x:xs)
lexer3 (x:xs) | isLower x = scanVSym [x] xs
  where scanVSym i "" = [VSym i]
        scanVSym i (x:xs) | (isLetter x || isDigit x || elem x "_'") = scanVSym (i++[x]) xs
                          | otherwise = VSym i : lexer3 (x:xs)

update :: (Vars,Value) -> Env -> Env
update (x,v) ((y,w):es) | x == y    = (x,v) : es
                        | otherwise = (y,w) : update (x,v) es
update (x,v) [] = [(x,v)]

-- The Parser
parser :: [Token] -> AExpr
parser input = sr input []

sr :: [Token] -> [Token] -> AExpr
sr (Err s : input) _ = error ("Lexical error: " ++ s)
sr [] [PA e] = e
sr input (VSym v : rs) = sr input (PA (Var v) : rs)   -- AExpr -> Var VSym
sr input (CSym n : rs) = sr input (PA (Const n) : rs) -- AExpr -> Const CSym
sr input (PA e2 : BOp AddOp : PA e1 : rs) = sr input (PA (Add e1 e2) : rs) -- AExpr -> AExpr AddOp AExpr
sr input (PA e2 : BOp SubOp : PA e1 : rs) = sr input (PA (Sub e1 e2) : rs) -- AExpr -> AExpr SubOp AExpr
sr input (PA e2 : BOp MulOp : PA e1 : rs) = sr input (PA (Mul e1 e2) : rs) -- AExpr -> AExpr MulOp AExpr
sr input (RPar : PA e : LPar : rs)        = sr input (PA e : rs)
sr (i:input) stack = sr input (i:stack)
sr [] stack = error (show stack)

testString = "(2*x-3*y)*(z+2)"
testLexed = lexer1 testString
testParsed = parser testLexed

parseString :: String -> AExpr
parseString = parser . lexer1

-- The main method
main :: IO ()
main = do
  putStrLn "Hello and Welcome"
  putStrLn "Enter an arithmetic expression"
  aexprInput <- getLine
  let aexpr = parser (lexer1 aexprInput)
  let loop env = do   -- this defines recursive locaal function loop :: Env -> IO ()
        putStrLn "Input a command"
        input <- getLine   -- String
        case words input of
          ("fv" : _) -> do
            putStrLn $ "The free variables of the expression are: " ++ show (fv aexpr)
            loop env
          ("square" : xstr : []) -> do
            let x = read xstr -- Integer
            putStrLn $ "The result is: " ++ show (x * x)
            loop env
          ("quit" : _) -> return ()
          (var : ":=" : val : []) ->
            loop (update (var,read val) env)
          ("env" : _) -> putStrLn ("The current environment is :" ++ show env)
                         >> loop env
          (["eval"]) -> putStrLn ("The value of the expression is: "
                                ++ show (eval env aexpr))
                         >> loop env
          _ -> putStrLn "Parse error!" >> loop env
  loop [("_",3)]

-- Earlier versions of main
--
-- main' :: IO ()
-- main' = do
--   putStrLn $ "Hello\n" ++ "Enter an integer"
--   x <- getLine -- getLine returns IO String, but x :: String
--   let y = read x -- want: y is an integer
--   putStrLn ("The result is: " ++ show (y * y))
--
-- main'' :: IO ()
-- main'' =
--   let welcome = putStrLn $ "Hello\n" ++ "Enter an integer" -- IO ()
--       x = getLine -- IO String
--       y = fmap read x -- IO Integer
--       res = y >>= (\z -> putStrLn $ "The result is: " ++ show (z * z))
--    in welcome >> res
--
-- main :: IO ()
-- main =  (putStrLn $ "Hello\n" ++ "Enter an integer") >>
--         (getLine >>=
--          (\x -> putStrLn $ "The result is: " ++ show (read x * read x)))
