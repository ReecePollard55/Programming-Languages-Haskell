import Distribution.Simple.Utils (xargs)
addLengths :: [String] -> Int
addLengths [] = 0
addLengths (x:xs) = length(x) + addLengths(xs)

findMaybe :: (a -> Bool) -> [a] -> Maybe a
findMaybe x [] = Nothing 
findMaybe x (y:ys) = if x y then Just y else findMaybe x ys

data BTree a = BLeaf a | BNode (BTree a) (BTree a)
    deriving Show 
myTree :: BTree Integer
myTree = BNode (BNode (BLeaf 3) (BLeaf 5))
            (BNode (BLeaf 1) (BLeaf (-3)))

sumTree :: BTree Integer -> Integer
sumTree (BLeaf x) = x
sumTree (BNode x y) = sumTree x + sumTree y

invertTree :: BTree Integer -> BTree Integer
invertTree (BLeaf x) = BLeaf (x*(-1)) 
invertTree (BNode x y) = BNode (invertTree x) (invertTree y) 

findPred :: (Integer -> Bool) -> BTree Integer -> Bool
findPred n (BLeaf x) = n x 
findPred n (BNode x y) = findPred n x || findPred n y