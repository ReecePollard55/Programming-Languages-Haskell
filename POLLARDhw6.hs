--Reece Pollard
--Worked with James, Dylan and Connor
type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop
                     | Not Prop | Imp Prop Prop | Iff Prop Prop
                     | Xor Prop Prop 
   deriving (Show,Eq)
 
prop1 = Var "X" `And` Var "Y" -- X /\ Y
prop2 = Var "X" `Imp` Var "Y" -- X -> Y
prop3 = Not (Var "X") `Or` Var "Y" -- !X \/ Y
prop4 = Not (Var "X") `Or` Not (Var "Y") -- !X \/!Y

data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp deriving (Show, Eq)

data Token = VSym Vars | CSym Bool | BOp BOps | NotOp
                       | LPar | RPar | PB Prop | Err String
   deriving (Show, Eq)

-- 1

preproc :: String -> String
preproc "" = ""
preproc ('(':xs) = ' ' : '(' : ' ' : preproc xs
preproc (')':xs) = ' ' : ')' : ' ' : preproc xs
preproc ('/':('\\':xs)) = ' ': '/':'\\' : ' ' :preproc xs
preproc ('\\':('/':xs)) = ' ': '\\':'/' : ' ' :preproc xs
preproc ('!':xs) = ' ' : '!': ' ' : preproc xs
preproc ('-':('>':xs)) = ' ': '-':'>' : ' ' :preproc xs
preproc ('<':('-':('>':xs))) = ' ':'<':'-':'>' : ' ' :preproc xs
preproc ('<':('+':('>':xs))) = ' ':'<':'+':'>' : ' ' :preproc xs
preproc('t':('t':xs)) = ' ' :'t':'t': ' ': preproc xs
preproc('f':('f':xs)) = ' ' :'f':'f': ' ': preproc xs
preproc (x:xs) = x : preproc xs

analyze :: [String] -> [Token]
analyze [] = []
analyze ("(":xs) = LPar : analyze xs
analyze (")":xs) = RPar : analyze xs
analyze ("/\\":xs) = BOp AndOp: analyze xs
analyze ("\\/":xs) = BOp OrOp: analyze xs
analyze ("!":xs) = NotOp : analyze xs
analyze ("->":xs) = BOp ImpOp: analyze xs
analyze ("<->":xs) = BOp IffOp: analyze xs
analyze ("<+>":xs) = BOp XorOp: analyze xs
analyze ("tt":xs) = CSym True : analyze xs
analyze ("ff":xs) = CSym False : analyze xs
analyze (x:xs) = VSym x : analyze xs

lexer :: String -> [Token]
lexer = analyze . words . preproc 

-- 2

parser :: [Token] -> Prop
parser input = sr input []

sr :: [Token] -> [Token] -> Prop
sr (Err s : input) _ = error ("Lexical Error: " ++ s)
sr [] [PB e] = e
sr input (VSym v : rs) = sr input (PB (Var v) : rs)
sr input (CSym n : rs) = sr input (PB (Const n) : rs)
sr input (PB e1 : NotOp : rs) = sr input (PB (Not e1) : rs)
sr input (PB e1 : BOp AndOp : PB e2 : rs) = sr input (PB (And e1 e2) : rs)
sr input (PB e1 : BOp OrOp : PB e2 : rs) = sr input (PB (Or e1 e2) : rs)
sr input (PB e1 : BOp ImpOp : PB e2 : rs) = sr input (PB (Imp e1 e2) : rs)
sr input (PB e1 : BOp IffOp : PB e2 : rs) = sr input (PB (Iff e1 e2) : rs)
sr input (PB e1 : BOp XorOp : PB e2 : rs) = sr input (PB (Xor e1 e2) : rs)
sr input (RPar : PB e : LPar : rs) = sr input (PB e : rs)
sr (i : input) stack = sr input (i : stack)
sr [] stack = error (show stack)

-- 3 and 4

removeDups :: (Eq a) => [a] -> [a]
removeDups = foldr (\x -> (x :) . filter (/= x)) []

fv :: Prop -> [Vars]
fv (Var x) = [x]
fv (And f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Or f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Imp f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Iff f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Xor f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Not f) = fv f
fv _ = []

eval :: [(Vars, Bool)] -> Prop -> Bool
eval env (Var x) = case lookup x env of
  Nothing -> error $ "No value for variable " ++ x
  Just v -> v
eval env (Const b) = b
eval env (And f1 f2) = eval env f1 && eval env f2
eval env (Or f1 f2) = eval env f1 || eval env f2
eval env (Imp f1 f2) = not (eval env f1) || eval env f2
eval env (Iff f1 f2) = if eval env f1 then eval env f2 else not (eval env f2)
eval env (Not f) = not (eval env f)
eval env (Xor f1 f2) = xor (eval env f1) (eval env f2)

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

evalAll :: Prop -> [[(Vars, Bool)]] -> Bool
evalAll = any . flip eval

genEnvs :: [Vars] -> [[(Vars, Bool)]]
genEnvs = foldr (\x y -> map ((x, True) :) y ++ map ((x, False) :) y) [[]]

match :: Prop -> Prop -> [[(Vars, Bool)]] -> Bool
match x y [] = True
match x y (z : zs)
  | eval z x == eval z y = match x y zs
  | otherwise = False

sat :: Prop -> Bool
sat p = evalAll p (genEnvs (fv p))

tauto :: Prop -> Bool
tauto x = all (`eval` x) (genEnvs (fv x))

contra :: Prop -> Bool
contra x = not (any (`eval` x) (genEnvs (fv x)))

equiv :: Prop -> Prop -> Bool
equiv x y = match x y (genEnvs (fv x))

main :: IO ()
main = do
  putStrLn "Enter a proposition:"
  propInput <- getLine
  let prop = parser (lexer propInput)
  let loop prop = do
        putStrLn "Enter next command:"
        input <- getLine
        case words input of
          ("fv" : _) -> do
            print (fv prop)
            loop prop
          ("sat" : _) -> do
            print (sat prop)
            loop prop
          ("tauto" : _) -> do
            print (tauto prop)
            loop prop
          ("contra" : _) -> do
            print (contra prop)
            loop prop
          ("equiv" : _) -> do
            putStrLn "Enter a second proposition"
            propInput2 <- getLine
            let prop2 = parser (lexer propInput2)
            print (equiv prop prop2)
            loop prop
          ("new" : _) -> do
            putStrLn "Enter new proposition"
            propInput <- getLine
            let prop = parser (lexer propInput)
            loop prop
          ("quit" : _) -> return ()
          _ -> putStrLn "Parse error!" >> loop prop
  loop prop
