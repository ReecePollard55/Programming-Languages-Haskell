--Reece Pollard
--Worked with Connor Lawson 

--Higher-order: first functions

--1.1 Currying and uncurrying 
--Problem 1
mapPair :: (a -> b -> c) -> [(a,b)] -> [c]
mapPair n = map (\(x,y) -> n x y)
--Problem 2
mapPair' :: (a -> b -> c) -> [(b,a)] -> [c]
mapPair' n = map (\(y,x) -> n x y)

--1.2 zipWith
--Problem 1
diff :: [Integer] -> [Integer] -> [Integer]
diff = zipWith (\x y -> x - y) 
--Problem 2
splice :: [String] -> [String] -> [String]
splice x y = zipWith (\x y -> x ++ y ++ x) x y 

--1.3 map
--Problem 1
sqLens :: [String] -> [Integer]
sqLens = map (\x -> fromIntegral (length x) ^ 2)
--Problem 2
bang :: [String] -> [String]
bang = map (\x -> x ++ "!") 

--1.4 filter
--Problem 1
digitsOnly :: [Integer] -> [Integer]
digitsOnly n = filter (\x -> (x > 0) && (x < 9)) n    
--Problem 2
removeXs :: [String] -> [String]
removeXs = filter (\x -> take 1 x /= "X")
  

--Higher-order: using folds

--1.
findNum :: Integer -> [Integer] -> Bool
findNum n [] = False
findNum n (x:xs) = if n == x then True else findNum n xs

findNum' :: Integer -> [Integer] -> Bool
findNum' n = foldr (\x -> (||) (n == x)) False

--2.
exists :: (a -> Bool) -> [a] -> Bool
exists n [] = False
exists n (x:xs) = (n x) || exists n xs

exists' :: (a -> Bool) -> [a] -> Bool
exists' n [] = False
exists' n xs = foldr ((||) . n) False xs

--3.
noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = if x `elem` xs then noDups xs else x: noDups xs

noDups' :: Eq a => [a] -> [a]
noDups' = foldr (\x y -> if x `elem` y then y else x : y) []

--4.
--Lexical error at character 'H'
countOverflow :: Integer -> [String] -> Integer 
countOverflow x [] = 0
countOverflow x (y:ys) = if fromIntegral (length y) > x then 1 + countOverflow x ys else countOverflow x ys

countOverflow' :: Integer -> [String] -> Integer
countOverflow' n = foldr (\x y -> if fromIntegral (length x) > n then 1 + y else y) 0

--5.
concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

concatList' :: [[a]] -> [a]
concatList' = foldr (\x y -> x ++ y) []

--6. 
bindList :: (a -> [b]) -> [a] -> [b]
bindList n [] = []
bindList n (x:xs) = n x ++ bindList n xs 

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' n = foldr (\x y -> n x ++ y) []