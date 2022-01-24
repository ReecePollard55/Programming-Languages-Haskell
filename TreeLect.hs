data List a = Empty | Cons a (List a)

--Lists
list1 :: List Integer 
list1 = Cons 1 (Cons 2 (Cons 3 Empty))

list1' :: [Integer]
list1' = 1:2:3:[] -- [1,2,3]

data Tree a = Leaf | Node a (Tree a) (Tree a)
   deriving Show

--Trees
tree1 :: Tree Integer 
tree1 = Node 3 (Node 2 (Node 7 Leaf Leaf)
                       (Node 1 Leaf Leaf))
               (Node 5 Leaf Leaf) 

tree2 :: Tree Integer 
tree2 = Node 3 (Node 9 (Node 7 Leaf Leaf)
                       (Node 1 Leaf Leaf))
               (Node 5 Leaf Leaf) 

tree3 :: Tree String 
tree3 = Node "root" (Node "node1" (Node "hello" Leaf Leaf)
                                (Node "world" Leaf Leaf))
                  (Node "node2" (Node "foo" Leaf Leaf)
                                (Node "bar" Leaf Leaf))

--depthFirstConcat
dfc :: Tree String -> String 
dfc Leaf           = ""
dfc (Node x t1 t2) = x ++ dfc t1 ++ dfc t2 

dfc' :: Tree String -> String 
dfc' Leaf           = ""
dfc' (Node x t1 t2) = x ++ " " ++ dfc t1 ++ dfc t2

--breadFirstConcat
bfc :: Tree String -> String 
bfc Leaf           = ""
bfc (Node x t1 t2) = bfc t1 ++ x ++ " " ++ bfc t2 

--addTree
addList :: [Integer] -> Integer
addList [] = 0
addList (x:xs) = x + addList xs 

addTree :: Tree Integer -> Integer 
addTree Leaf           = 0 
addTree (Node x t1 t2) = x + addTree t1 + addTree t2

--Height of tree
height :: Tree a -> Integer 
height Leaf           = 0
height (Node x t1 t2) = 1 + max (height t1) (height t2) 

--Counte the Nodes
countNodes :: Tree a -> Integer 
countNodes Leaf           = 0
countNodes (Node x t1 t2) = 1 + height t1 + height t2

--Find if nodes are even
findEven :: Tree Integer -> Bool 
findEven Leaf           = False
findEven (Node x t1 t2) = if even x then True else findEven t1 || findEven t2 

findEven' :: Tree Integer -> Bool 
findEven' Leaf           = False
findEven' (Node x t1 t2) = even x || findEven t1 || findEven t2

--fold :: (a -> b -> b) -> b -> [a] -> bfc
foldTree :: (a -> b -> b -> b) -> b  -> Tree a -> b
foldTree rf bc (Leaf) = bc 
foldTree rf bc (Node x t1 t2) = rf x (foldTree rf bc t1) (foldTree rf bc t2)

--data types
data BTree a   = BLeaf a | BNode   (BTree a) (BTree a)
data LTree a   = LLeaf a | LNode a (LTree a) (LTree a)
data GTree a b = GLeaf a | GNode b (GTree a b) (GTree a b)

--