{-
    Takes in a list and returns a list of the elements from input list that are prime
    For Example:
    makePrimes [1..10] = [2,3,5,7]
    makePrimes [1,4,6,8,9,10] = []
    makePrimes [] = []
-}
makePrimes :: Integral a => [a] -> [a]
makePrimes [] = []
makePrimes xs = 
        if isPrime (head xs)
            then head xs : makePrimes (tail xs)
            else makePrimes (tail xs)

{-
    Takes in and integer value and return True Iff the integer is prime.  False otherwise.
    For Example: 
        isPrime 7 = True;
        isPrime 100 = False;
-}
--Check if a number is prime            
isPrime :: Integral t => t -> Bool 
isPrime 1 = False
isPrime 2 = True
isPrime n | not (null ([x | x <- [2 .. n - 1], mod n x == 0])) = False 
    | otherwise = True


{-
    Takes a list of elements and returns a list where every element within 
    the list is duplicated. For example:
        stutter [] = []
        stutter [1,2,3] = [1,1,2,2,3,3]
        stutter "Hello World" = "HHeelllloo  WWoorrlldd"
-}
stutter :: Eq a => [a] -> [a]
stutter [] = []
stutter (x:xs) = x : x : stutter xs


{- 
    Takes in a list and eliminate all consecutive duplicate elements. For example:
        compress "HHeelllloo  WWoorrlldd" = "Helo World"
        compress [1,2,2,3,3,3,4,4,4,4,1,1] = [1,2,3,4,1]
-}
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress ls =
    if head ls /= head (tail ls)
        then head ls : compress (tail ls)
        else compress (tail ls)

{-
    Takes two lists as arguments and returns True iff the first list is a suffix of the second.
    For example:
        isSuffixOf "bar" "" = False
        isSuffixOf "" "foo" = True
        isSuffixOf "bar" "foobar" = True
        isSuffixOf "bar" "foobarf" = False
-}
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] ls2 = True
isSuffixOf ls1 [] = False
isSuffixOf ls1 ls2 =
    if head ls1 == head ls2
        then  if ((tail ls1) == []) && ((tail ls2) /= [])
                then False
                else isSuffixOf (tail ls1) (tail ls2)
        else isSuffixOf ls1 (tail ls2)

{-
    Takes a list and returns True iff the list is in increasing sorted order and False otherwise.
    For example:
        increasing [1,2,3] = True
        increasing "ABCD" = True
        increasing [100,99..0] = False
-}
increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing xs = (head xs < head (tail xs)) && increasing (tail xs)

{-
    Increasing using a List Comprehension
-}
increasing' :: Ord a => [a] -> Bool
increasing' [] = True;
increasing' [x] = True;
increasing' (x:xs) = and [x <= y | (x,y) <- zip xs (tail xs)]

{-
    Calculates the McCarthy 91 function.
    The function is defined as follows:
    MC(n)=
        n - 10         & if n >  100
        MC(MC(n + 11)) & if n <= 100
    For Example:
        map mc91 [95..110] = [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
-}
mc91 :: Int -> Int
mc91 0 = 0
mc91 n = 
    if n > 100 
        then n - 10 
        else mc91(mc91 (n+11))

{-
    Takes a predicate function and two lists and returns a list which contains elements from the second
    list where the predicate returned True when applied to the corresponding index in the first list. 
    If the lists are not the same length then only consider indexes up to the length of the first list argument. 
    For example:
        select even [1..26] "abcdefghijklmnopqrstuvwxyz" = "bdfhjlnprtvxz"
        select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] = [1,2,3,4,5,6,7]
        select odd [1..10] "hello world this is joe" = "hlowr"
-}
select :: (a -> Bool) -> [a] -> [b] -> [b]
select pred is [] = []
select pred [] is = []
select pred is xs = 
    if pred (head is)
        then head xs : select pred (tail is) (tail xs)
        else select pred (tail is) (tail xs)

{-
    Takes a list of numbers as its argument and returns a list of sums of all prefixes of the list. 
    For example:
        prefixSum [1..10] = [1,3,6,10,15,21,28,36,45,55]
        prefixSum [2, 5] = [2,7]
-}
prefixSum :: [Int] -> [Int]
prefixSum [] = []
prefixSum xs = scanl1 (+) xs

{-
    Takes a predicate and a list and returns the list of elements that satisfies the predicate.
    For Example:
        myTakeWhile (odd) [1..10] = [1]
        myTakeWhile (odd) [2,4,6,7,8,9,10] = []
-}

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred [] = []
myTakeWhile pred (x:xs) = 
    if (pred x)
        then x : myTakeWhile pred xs
        else myTakeWhile pred [] 

{-
    Takes a predicate and a list and returns a pair of lists where the first part is 
    what takeWhile returns and the second part is the rest of the list.
    For Example:
        mySpan (odd) [2,4,6,7,8,9,10] = ([],[2,4,6,7,8,9,10])
-}
mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan pred xs = (takeWhile pred xs,dropWhile pred xs)

{-
    Takes an Integral k and a list of Ords.  Returns a list of length k lists representing all possible 
    combinations of length k.
    For Example:
        combinations 2 "ABCDE" = ["AB","AC","AD","AE","BC","BD","BE","CD","CE","DE"]
-}
combinations :: (Ord a, Integral b) => b -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

{-
    Takes a list and returns a list of length 3 lists representing all combinations of length 3 
    of the input list.
    For Example:
        combinations3 "ABCDE" = ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","EDE","CDE"]
-}
combinations3 :: Ord a => [a] -> [[a]]
combinations3 [] = []
combinations3 ls = combinations 3 ls

{-
    Defines a new data type to describe a tree.
-}
data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Show, Eq)

{-
    Takes a non-empty list and converts it into a balanced tree.
    For Example:
        balance [1,2] = NodeT (LeafT 1) (LeafT 2)
        balance [1,2,3,4] = NodeT (NodeT (LeafT 1) (LeafT 2)) (NodeT (LeafT 3) (LeafT 4))
-}
balance :: [a] -> Tree a
balance [x] = LeafT x
balance xs  = NodeT (balance (fst (splits xs))) (balance (snd (splits xs))) 

{-
    Takes a list and splits it in half.  Returns a pair of lists where the first part is 
    the first half of the list and the second part is the second half of the list.
    For Example:
        splits [1,2] = ([1],[2])
        splits [1,2,3,4,5] = ([1,2],[3,4,5]) 
-}
splits :: [a] -> ([a],[a])
splits xs = splitAt (quot (length xs) 2) xs


{-
    Defines a new data type to represent a binary tree
-}
data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

{-
    Takes a Binary tree T and outputs all paths P, from root of the tree 
    to each of its sub-trees
    For Example:
        allpaths Node Leaf (Node Leaf Leaf) = [This,GoLeft This,GoRight This,GoRight (GoLeft This),GoRight (GoRight This)]
-}
allpaths :: T -> [P] 
allpaths Leaf = [This]
allpaths (Node a b) = This : map GoLeft (allpaths a) ++ map GoRight (allpaths b)

{-
    Datatype to represent complex numbers
-}
data Complex = Complex { real :: Integer, imaginary :: Integer }
   
instance Show Complex where
    show (Complex real imaginary) = show (real) ++ "+" ++ show (imaginary) ++ "i"

instance Eq Complex where
    Complex c1 i1 == Complex c2 i2 = c1 == c2 && i1 == i2

instance Num Complex where
    (Complex c1 i1) + (Complex c2 i2) = Complex (c1 + c2) (i1 + i2)
    (Complex c1 i1) * (Complex c2 i2) = Complex ((c1 * c2)-(i1 * i2)) ((c1 * i2) + (c2 * i1))
 
