--Reece Pollard
--Worked with Connor Lawson
import Data.Char

type Vars = String

data AExpr = Var Vars | Const Integer
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
    deriving Show

data BExpr = TT | FF -- the true and false constants
           | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
           | Eql AExpr AExpr -- equality of arithmetic expressions
           | Lt AExpr AExpr -- true if the first is less than the second
           | Lte AExpr AExpr -- true if it’s less than or equal to
    deriving Show

data Instr = Assign Vars AExpr -- assign X to the value of an expression
           | IfThenElse BExpr Instr Instr -- conditional
           | While BExpr Instr -- looping construct
           | Do [Instr] -- a block of several instructions
           | Nop -- the "do nothing" instruction
    deriving Show

type Program = [Instr]

type Env = [ (Vars,Integer)]

-- update (x,v) e sets the value of x to v and keeps other variables in e the same
update :: (Vars, Integer) -> Env -> Env
update (x, y) [] = [(x,y)]
update (x, y) ((z, zs) : envs) = if x == z then (x, y) : envs 
                                 else (z, zs) : update (x, y) envs

lookUp :: Vars -> Env -> Integer
lookUp x [] = 0
lookUp x ((y,z):xs) = if x == y then z else lookUp x xs

-- Question 1.
evala env (Var x) = lookUp x env
evala env (Const x) = x
evala env (Add t1 t2) = evala env t1 + evala env t2
evala env (Sub t1 t2) = evala env t1 - evala env t2
evala env (Mul t1 t2) = evala env t1 * evala env t2
evala env (Div t1 t2) = evala env t1 `div` evala env t2

evalb :: Env -> BExpr -> Bool
evalb env TT = True
evalb env FF = False
evalb env (And t1 t2) = evalb env t1 && evalb env t2
evalb env (Or t1 t2) = evalb env t1 || evalb env t2
evalb env (Not t1) = not (evalb env t1)
evalb env (Eql t1 t2) = evala env t1 == evala env t2
evalb env (Lt t1 t2) = evala env t1 < evala env t2
evalb env (Lte t1 t2) = evala env t1 <= evala env t2

--Question 2.
exec :: Instr -> Env -> Env
exec (Assign x t1) y = update (x, z) y where z = evala y t1 
exec (IfThenElse x t1 t2) y = if evalb y x then exec t1 y else exec t2 y
exec (While x t1) y | evalb y x = exec (Do [t1, While x t1]) y
                    | otherwise = y
exec (Do []) y = y
exec (Do (x:xs)) y = exec (Do xs) (exec x y)
exec Nop y = y

run :: Program -> Env
run p = exec (Do p) []

sum100 :: Program -- a program to add together all the numbers up to 100
sum100 = [
  Assign "X" (Const 0), -- initialize the sum at X=0
  Assign "C" (Const 1), -- initialize the counter at C=1
  While (Lt (Var "C") (Const 101)) -- while C < 101, do:
        (Do [Assign "X" (Add (Var "X") (Var "C")), -- X := X + C;
             Assign "C" (Add (Var "C") (Const 1))] -- C := C + 1
        )]

--Question 3.
data UOps = NotOp deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp
          | AndOp | OrOp | EqlOp | LtOp | LteOp | AssignOp
    deriving Show
data Token = VSym String | CSym Integer | BSym Bool
           | UOp UOps | BOp BOps
           | LPar | RPar | LBra | RBra | Semi
           | Keyword String
           | Err String
           | PA AExpr | PB BExpr | PI Instr
           | Block [Instr]
    deriving Show

isCSym :: String -> Bool
isCSym = let q0 "" = False
             q0 (x:xs) = isDigit x && q1 xs
             q1 "" = True
             q1 (x:xs) = isDigit x && q1 xs
          in q0

isVSym :: String -> Bool
isVSym = let q0 ""     = False
             q0 (x:xs) | x `elem` ['a'..'z'] = q1 xs
                       | otherwise = False
             q1 [] = True
             q1 (x:xs) = (isLetter x || isDigit x || elem x "_'") && q1 xs
          in q0

preproc :: String -> String
preproc "" = ""
preproc ('/' : '\\' : xs) = " /\\ " ++ preproc xs
preproc ('\\' : '/' : xs) = " \\/ " ++ preproc xs
preproc ('!' : xs) = " ! " ++ preproc xs
preproc ('*' : xs) = " * " ++ preproc xs
preproc ('/' : xs) = " / " ++ preproc xs
preproc ('+' : xs) = " + " ++ preproc xs
preproc ('-' : xs) = " - " ++ preproc xs
preproc ('<' : '=' : xs) = " <= " ++ preproc xs
preproc ('<' : xs) = " < " ++ preproc xs
preproc ('(' : xs) = " ( " ++ preproc xs
preproc (')' : xs) = " ) " ++ preproc xs
preproc ('{' : xs) = " { " ++ preproc xs
preproc ('}' : xs) = " } " ++ preproc xs
preproc (';' : xs) = " ; " ++ preproc xs
preproc (':' : '=' : xs)  = " := " ++ preproc xs
preproc ('=' : '=' : xs)  = " == " ++ preproc xs
preproc (x : xs) = x : preproc xs

classify :: String -> Token
classify "while" = Keyword "while"
classify "if"    = Keyword "if"
classify "then"  = Keyword "then"
classify "else"  = Keyword "else"
classify "nop"   = Keyword "nop"
classify "("   = LPar
classify ")"   = RPar
classify "/\\" = BOp AndOp
classify "\\/" = BOp OrOp
classify "!"   = UOp NotOp
classify "+"   = BOp AddOp
classify "-"   = BOp SubOp
classify "*"   = BOp MulOp
classify "/"   = BOp DivOp
classify "=="  = BOp EqlOp
classify "<"   = BOp LtOp
classify "<="  = BOp LteOp 
classify ":="  = BOp AssignOp
classify "T"   = BSym True
classify "F"   = BSym False
classify "{"   = LBra
classify "}"   = RBra
classify ";"   = Semi
classify s | isCSym s = CSym (read s)
classify s | isVSym s = VSym s
classify _ = Err "Error!"

lexer :: String -> [Token]
lexer x = map classify (words (preproc x))

--Question 4
parser :: [Token] -> Instr
parser input = sr ([LBra] ++ input ++ [RBra]) []

sr :: [Token] -> [Token] -> Instr
sr (Err s : input) _ = error ("Lexical error: " ++ s)
sr [] [PI e] = e
sr input (PI e2 : PB e1 : Keyword "while" : rs) = sr input (PI (While e1 e2) : rs)
sr input (PI e3 : Keyword "else" : PI e2 : Keyword "then" : PB e1 : Keyword "if" : rs) = sr input (PI (IfThenElse e1 e2 e3) : rs)
sr input (Keyword "nop" : rs) = sr input (PI Nop : rs)
sr input (VSym v : rs) = sr input (PA (Var v) : rs)
sr input (CSym n : rs) = sr input (PA (Const n) : rs)
sr input (BSym b : rs) = if b then sr input (PB TT : rs) else sr input (PB FF : rs)
sr input (PA e2 : BOp AddOp : PA e1 : rs) = sr input (PA (Add e1 e2) : rs) 
sr input (PA e2 : BOp SubOp : PA e1 : rs) = sr input (PA (Sub e1 e2) : rs)
sr input (PA e2 : BOp MulOp : PA e1 : rs) = sr input (PA (Mul e1 e2) : rs)
sr input (PA e2 : BOp DivOp : PA e1 : rs) = sr input (PA (Div e1 e2) : rs)
sr input (PB e2 : BOp AndOp : PB e1 : rs) = sr input (PB (And e1 e2) : rs)
sr input (PB e2 : BOp OrOp : PB e1 : rs) = sr input (PB (Or e1 e2) : rs)
sr input (PB e1 : UOp NotOp : rs) = sr input (PB (Not e1) : rs)
sr input (PA e2 : BOp EqlOp : PA e1 : rs) = sr input (PB (Eql e1 e2) : rs)
sr input (PA e2 : BOp LtOp : PA e1 : rs) = sr input (PB (Lt e1 e2) : rs)
sr input (PA e2 : BOp LteOp : PA e1 : rs) = sr input (PB (Lte e1 e2) : rs)
sr input (PA e2 : BOp AssignOp : PA (Var e1) : rs) = sr input (PI (Assign e1 e2) : rs)
sr input (RPar : PA e : LPar : rs) = sr input (PA e : rs)
sr input (RPar : PB e : LPar : rs) = sr input (PB e : rs)
sr input (RBra : PI e : rs) = sr input (Block [e] : rs)
sr input (RBra : Semi : PI e : rs) = sr input (Block [e] : rs)
sr input (Block bl : Semi : PI e : rs) = sr input (Block (e:bl) : rs)
sr input (Block bl : LBra : rs) = sr input (PI (Do bl) : rs)
sr (i:input) stack = sr input (i:stack)
sr [] stack = error (show stack)

--Question 5
main ::IO ()
main = do 
    x <- getLine
    input <- readFile x
    let y = lexer input
    let z = parser y
    let r = run [z]
    putStrLn (show r)