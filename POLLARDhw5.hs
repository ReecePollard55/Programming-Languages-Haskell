-- Reece Pollard
-- Worked with James
import Control.Applicative

--Problem 1.1
data Comp a = Value a | Error String
    deriving Show

instance Functor Comp where
    fmap f (Value a) = Value (f a)
    fmap f _ = error "error"

unitComp :: a -> Comp a
unitComp b = Value b

joinComp :: Comp (Comp a) -> Comp a
joinComp (Value (Value z)) = Value z
joinComp _ = error "error"

bindComp :: (a -> Comp b) -> Comp a -> Comp b
bindComp f = joinComp . (fmap f)

applyComp :: Comp (a -> b) -> Comp a -> Comp b
applyComp fs as = bindComp (\f -> fmap f as) fs

instance Applicative Comp where
    pure = unitComp
    fs <*> as = applyComp fs as

instance Monad Comp where
    return = unitComp
    as >>= f = bindComp f as

--Problem 1.2
data Prop a = Var a | TT | FF | And (Prop a) (Prop a) | Or (Prop a) (Prop a)
               | Neg (Prop a) | Imp (Prop a) (Prop a) | Iff (Prop a) (Prop a)
    deriving Show

instance Functor Prop where 
    fmap f (Var p) = Var (f p)
    fmap f TT = TT
    fmap f FF = FF
    fmap f (And a b) = And (fmap f a) (fmap f b)
    fmap f (Or a b) = Or (fmap f a) (fmap f b)
    fmap f (Neg a) = Neg (fmap f a)
    fmap f (Imp a b) = Imp (fmap f a) (fmap f b)
    fmap f (Iff a b) = Iff (fmap f a) (fmap f b)

unitProp :: a -> Prop a
unitProp x = Var x

joinProp :: Prop (Prop a) -> Prop a
joinProp (Var a) = a
joinProp TT = TT
joinProp FF = FF
joinProp (And a b) = And (joinProp a)(joinProp b)
joinProp (Or a b) = Or (joinProp a)(joinProp b)
joinProp (Neg a) = Neg (joinProp a)
joinProp (Imp a b) = Imp (joinProp a)(joinProp b)
joinProp (Iff a b) = Iff (joinProp a)(joinProp b)

bindProp :: (a -> Prop b) -> Prop a -> Prop b
bindProp f = joinProp . fmap f

applyProp :: Prop (a -> b) -> Prop a -> Prop b
applyProp fs as = bindProp (\f -> fmap f as) fs

instance Applicative Prop where
    pure = unitProp
    fs <*> as = applyProp fs as

instance Monad Prop where
    return = unitProp
    as >>= f = bindProp f as

--Problem 1.3
data Poly a = Mono Double [a] | Sum (Poly a) (Poly a)
    deriving Show

poly1 = Mono 1.0 [] -- 1
poly2 = Mono 2.0 ["X", "X"] -- 2x^2
poly3 = Sum (Mono 3.0 ["Y", "Y"]) (Mono (-0.5) ["X","Y"]) -- 3y^2-xy/2
poly4 = Mono 2.0 ["X"]

instance Functor Poly where
    fmap f (Mono c x) = Mono c (fmap f x)
    fmap f (Sum a b) = Sum (fmap f a) (fmap f b)

unitPoly :: a -> Poly a
unitPoly x = Mono 1 [x]

joinPoly :: Poly (Poly a) -> Poly a
joinPoly (Sum a b) = Sum (joinPoly a) (joinPoly b)
--joinPoly a b = mult2poly a b

--mult2poly :: Poly a -> Poly a -> Poly a
--mult2poly a b -> foldr (\x y -> x * y) a b

bindPoly :: (a -> Poly b) -> Poly a -> Poly b
bindPoly f = joinPoly . fmap f

applyPoly :: Poly (a -> b) -> Poly a -> Poly b
applyPoly fs as = bindPoly (\f -> fmap f as) fs

instance Applicative Poly where
    pure = unitPoly
    fs <*> as = applyPoly fs as

instance Monad Poly where
    return = unitPoly
    as >>= f = bindPoly f as

--Problem 2
type Vars = String -- variables
type Vals = Double -- values
type Env = [(Vars,Vals)] -- environments

env0 :: Env
env0 = [("X", 10.0), ("Y",2.0)]

lookUp :: Vars -> Env -> Vals
lookUp x [] = error ("No value for variable " ++ x)
lookUp x ((y,v):es) = if y == x then v else lookUp x es

evalPoly :: Env -> Poly Vars -> Vals
evalPoly env (Mono d []) = d
evalPoly env (Mono d (x:xs)) = (lookUp x env) * evalPoly env (Mono d xs)
evalPoly env (Sum p1 p2) = evalPoly env p1 + evalPoly env p2

--Problem 3
main :: IO ()
main = do 
    putStr "Enter a number: \n"
    line <- getLine
    let x = (read line :: Double)
    let z = [("X", x)]
    putStr "The result is: "
    print (evalPoly z poly2)