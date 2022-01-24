--Worked with Connor Lawson
--PART 1

--1
minList :: [Integer] -> Integer 
minList [] = error "Empty List"
minList (x:xs) = if x < head xs then x else minList xs

--2
multiplyList :: [Integer] -> Integer 
multiplyList [] = 1
multiplyList (x:xs) = x * multiplyList xs

--3
existsOdd :: [Integer] -> Bool 
existsOdd [] = False 
existsOdd (x:xs) = if odd x then True else existsOdd xs

--4
findOdd :: [Integer] -> Maybe Integer 
findOdd [] = Nothing 
findOdd (x:xs) = if odd x then Just x else findOdd xs

--5
removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs) = if null x then removeEmpty xs else x : removeEmpty xs

--6
catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (x:xs) = case x of 
                       Nothing -> catMaybes xs
                       Just y -> y : catMaybes xs

--7
collect :: [Either a b] -> ([a],[b])
collect [] = ([],[])
collect (x:xs) = case x of 
                      Left y -> (y:a,b)
                           where (a,b) = collect xs
                      Right y -> (a,y:b)
                           where (a,b) = collect xs

--8
isPrefix :: (Eq a) => [a] -> [a] -> Bool 
isPrefix [] [] = True 
isPrefix [] _  = True 
isPrefix _ []  = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys 

--9
findIndex :: Integer -> [Integer] -> Maybe Integer 
findIndex n xs = findHelper n xs 0

findHelper :: Integer -> [Integer] -> Integer -> Maybe Integer 
findHelper n [] c = Nothing 
findHelper n (x:xs) c | n == x = Just x  
                     | otherwise = findHelper n xs (c + 1)

--10
repeatInt :: a -> Integer -> [a]
repeatInt x 0 = [] 
repeatInt x y = x : repeatInt x (y-1)

--PART 2

--1
addIndex :: [a] -> [(Integer, a)]
addIndex xs = let addIndex' i [] = []
                  addIndex' i (x:xs) = (i,x) : addIndex' (i+1) xs
              in  addIndex' 0 xs

--2
swapAll :: [(a,b)] -> [(b,a)]
swapAll [] = []
swapAll ((x,y):xs) = (y,x) : swapAll xs

--3
findDouble :: Eq a => [(a,a)] -> Maybe a
findDouble [] = Nothing 
findDouble ((x,y):zs) | x == y = Just x
                      | True = findDouble zs

--4
defined :: Maybe a -> Bool 
defined Nothing = False 
defined (Just x) = True 

--5
skip :: [a] -> [a] 
skip [] = []
skip [x] = [x]
skip (x:(y:ys)) = x : skip ys 

--6
removeEvens :: [Integer] -> [Integer]
removeEvens [] = [] 
removeEvens (x:xs) = if even x then removeEvens xs else x : removeEvens xs 

--7
doubleAll :: [Integer] -> [Integer]
doubleAll (x:xs) = 2 * x : doubleAll xs 

--8
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs 

--9
countInt :: Integer -> [Integer] -> Integer 
countInt x [] = 0
countInt x (y:ys) | x==y = 1 + countInt x ys 
                  | otherwise = countInt x ys 

--10
countEq :: Eq a => a -> [a] -> Integer 
countEq x [] = 0 
countEq x (y:ys) | x==y = 1 + countEq x ys 
                 | otherwise = countEq x ys
