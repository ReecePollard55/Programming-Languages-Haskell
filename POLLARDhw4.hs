--Reece Pollard
--Worked with Connor Lawson 

--Data type of Tree
data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
    deriving Show

--Part 1
getLeaves :: LTree a -> [a]
getLeaves (LLeaf a) = [a]
getLeaves (LNode a t1 t2) = getLeaves t1 ++ getLeaves t2

countNodes :: LTree a -> Integer
countNodes (LLeaf a) = 0
countNodes (LNode a t1 t2) = 1 + countNodes t1 + countNodes t2

minTree :: LTree Integer -> Integer
minTree (LLeaf a) = a
minTree (LNode a t1 t2) | a < min (minTree t1) (minTree t2) = a
                        | otherwise = min (minTree t1) (minTree t2)

occursInLeaves :: (a -> Bool) -> LTree a -> Bool
occursInLeaves x (LLeaf a) = x a
occursInLeaves x (LNode a t1 t2) = occursInLeaves x t1 || occursInLeaves x t2

checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover x (LLeaf a) = a == x
checkNoCover x (LNode a t1 t2) = a /= x && (checkNoCover x t1 || checkNoCover x t2)


--Part 2
foldTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode y t1 t2) = comb y (foldTree comb base t1)
                                            (foldTree comb base t2)

getLeaves' :: LTree a -> [a]
getLeaves' = foldTree (\x t1 t2 -> t1 ++ t2)(: [])

countNodes' :: LTree a -> Integer
countNodes' = foldTree(\x t1 t2 -> 1 + t1 + t2)(\x -> 0)

minTree' :: LTree Integer -> Integer
minTree' = foldTree (\x t1 t2 -> min x (min t1 t2))(\x -> x)

occursInLeaves' :: (a -> Bool) -> LTree a -> Bool
occursInLeaves' = foldTree(\x t1 t2 -> t1 || t2)

checkNoCover' :: (Eq a) => a -> LTree a -> Bool
checkNoCover' x = foldTree (\a left right -> a /= x && (left || right)) (== x)