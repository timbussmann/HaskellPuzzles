-- https://wiki.haskell.org/99_questions/1_to_10

import Test.HUnit
import Data.List

-- problem1
problem1 = TestList [
    TestCase $ assertEqual "with ints" 4 (myLast [1,2,3,4]),
    TestCase $ assertEqual "with chars" 'z' (myLast ['x', 'y', 'z'])
    ]

myLast :: [a] -> a
--myLast x = last x
--myLast (x:[]) = x
myLast [x] = x
myLast (_:xs) = myLast xs

-- problem2
problem2 = TestList [
    TestCase $ assertEqual "with ints" 3 (myButLast [1,2,3,4]),
    TestCase $ assertEqual "with chars" 'y' (myButLast ['a'..'z'])
    ]

myButLast :: [a] -> a
--myButLast = last . init

--myButLast (x : _ : []) = x
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

-- problem3
problem3 = TestList [
    TestCase $ assertEqual "with ints" 2 (myElementAt [1,2,3] 2),
    TestCase $ assertEqual "with chars" 'e' (myElementAt "haskell" 5)
    ]

myElementAt :: [a] -> Int -> a
--myElementAt xs i = xs !! (i - 1)
myElementAt (x:_) 1 = x
myElementAt (_:xs) n = myElementAt xs (n - 1)

-- problem4
problem4 = TestList [
    TestCase $ assertEqual "with ints" 3 (myLength [123, 456, 789]),
    TestCase $ assertEqual "with chars" 13 (myLength "Hello, world!")
    ]

myLength :: [a] -> Int
--myLength [] = 0
--myLength (_:xs) = 1 + myLength xs
myLength = fst . last . zip [1..]

-- problem5
problem5 = TestList [
    TestCase $ assertEqual "with ints" [4,3,2,1] (myReverse [1,2,3,4]),
    TestCase $ assertEqual "with chars" "!amanap ,lanac a ,nalp a ,nam A" (myReverse "A man, a plan, a canal, panama!")
    ]

myReverse :: [a] -> [a]
--myReverse [] = []
--myReverse (x:xs) = myReverse xs ++ [x]

--myReverse = foldl (\result x -> x : result) []
myReverse = foldl (flip (:)) []

-- problem6
problem6 = TestList [
    TestCase $ assertEqual "no palindrome" False (isPalindrome [1,2,3]),
    TestCase $ assertEqual "string palindrome" True (isPalindrome "madamimadam"),
    TestCase $ assertEqual "int palindrome" True (isPalindrome [1,2,4,8,16,8,4,2,1])
    ]

isPalindrome :: (Eq a) => [a] -> Bool
-- isPalindrome input = input == reverse input
isPalindrome [] = True
isPalindrome (x:xs) = last xs == x && isPalindrome (init xs)

-- problem7
data NestedList a = Elem a | List [NestedList a]

problem7 = TestList [
    TestCase $ assertEqual "single item" [5] (myFlatten (Elem 5)),
    TestCase $ assertEqual "nested list" [1,2,3,4,5] (myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    ]

myFlatten :: NestedList a -> [a]
myFlatten (List xs) = foldl (\result x -> result ++ (myFlatten x)) [] xs
myFlatten (Elem x) = [x]

-- problem8
problem8 = TestList [
    TestCase $ assertEqual "with chars" "abcade" (compress "aaaabccaadeeee"),
    TestCase $ assertEqual "with ints" [1,2,1] (compress [1, 1, 1, 2, 2, 1, 1])
    ]

compress :: (Eq a) => [a] -> [a]
--compress (x1 : x2 : xs) = if x1 == x2 then x1 : compress xs else x1 : compress (x2:xs)
--compress xs = xs
compress = (map head) . group

-- problem9
problem9 = TestList [
    TestCase $ assertEqual "with chars" ["aaaa","b","cc","aa","d","eeee"] (myPack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
    ]

myPack :: (Eq a) => [a] -> [[a]]
--myPack = group

-- myPack (all@(x:_)) = let res = span (==x) all
--                      in fst res : myPack (snd res)
-- myPack [] = []

myPack xs = let (lastElement, resultList) = foldr addToList ([],[]) xs
            in lastElement : resultList
    where addToList x (current@(c:_), result) = if x == c
                                                then (x:current, result)
                                                else ([x], current : result)
          addToList x ([], result) = ([x], result)

-- problem10
problem10 = TestList [
    TestCase $ assertEqual "with chars" [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] (myEncode "aaaabccaadeeee")
    ]

myEncode :: (Eq a) => [a] -> [(Int, a)]
--myEncode [] = []
--myEncode input@(x:xs) = let (same, rest) = span (==x) input
--                        in (length same, head same) : myEncode rest

myEncode input = [(length x, head x) | x <- myPack input]
