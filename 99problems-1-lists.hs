-- https://wiki.haskell.org/99_questions/1_to_10
-- https://wiki.haskell.org/99_questions/11_to_20
-- https://wiki.haskell.org/99_questions/21_to_28

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

-- problem14
problem14 = TestList [
    TestCase $ assertEqual "should duplicate elements" [1,1,2,2,3,3] (myDuplicate [1,2,3])
    ]

myDuplicate :: [a] -> [a]
myDuplicate [] = []
myDuplicate (x:xs) = x : x : myDuplicate xs

-- problem15
problem15 = TestList [
    TestCase $ assertEqual "should replicate elements" "aaabbbccc" (myReplicate "abc" 3)
    ]

myReplicate :: [a] -> Int -> [a]
--myReplicate elements r = foldr (\e result -> add e r result) [] elements
--    where   add element 0 result = result
--            add element r result = element : add element (r - 1) result

myReplicate elements r = concatMap (take r . repeat) elements

-- problem16
problem16 = TestList [
    TestCase $ assertEqual "should drop elements" "abdeghk" (myDrop "abcdefghik" 3)
    ]

myDrop :: [a] -> Int -> [a]
-- myDrop xs n = drop xs 1
--     where drop (x:xs) c
--             | c == n = drop xs 1
--             | otherwise = x : (drop xs (c + 1))
--           drop [] c = []

myDrop xs n = (map snd . filter (\e -> (fst e) /= n) . zip (cycle [1..n])) xs

-- problem17
problem17 = TestList [
    TestCase $ assertEqual "should split list in two parts" ("abc", "defghik") (mySplit "abcdefghik" 3)
    ]

mySplit :: [a] -> Int -> ([a], [a])
mySplit xs 0 = ([], xs)
mySplit (x:xs) c =  let (a,b) = mySplit xs (c - 1)
                    in ((x:a), b)

-- problem18
problem18 = TestList [
    TestCase $ assertEqual "should take elements between i'th and k'th element" "defg" (mySlice "abcdefghik" 4 7)
    ]

mySlice :: [a] -> Int -> Int -> [a]
mySlice xs i k = take (k - i') (drop i' xs)
                    where i' = i - 1

-- problem19
problem19 = TestList [
    TestCase $ assertEqual "should rotate elements to the right" "defghabc" $ myRotate "abcdefgh" 3,
    TestCase $ assertEqual "should rotate elements to the left" "ghabcdef" $ myRotate "abcdefgh" (-2)
    ]

myRotate :: [a] -> Int -> [a]
myRotate xs 0 = xs
myRotate a@(x:xs) n
    | n > 0 = myRotate (xs ++ [x]) (n - 1)
    | n < 0 = myRotate a (length a + n)

-- problem20
problem20 = TestList [
    TestCase $ assertEqual "should remove n'th element from the result" ('b', "acd") $ myRemoveAt 2 "abcd"
    ]

myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt 1 (x:xs) = (x, xs)
myRemoveAt n (x:xs) = let (a, b) = myRemoveAt (n - 1) xs in (a, x:b)

-- problem21
problem21 = TestList [
    TestCase $ assertEqual "should insert item at position" "aXbcd" $ myInsertAt 'X' "abcd" 2
    ]

myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt c xs 1 = c : xs
myInsertAt c (x:xs) n = x : (myInsertAt c xs (n - 1))

-- problem22
problem22 = TestList [
    TestCase $ assertEqual "create integers within a given range" [4,5,6,7,8,9] $ myRange 4 9
    ]

myRange :: Int -> Int -> [Int]
--myRange start end = [start..end]
myRange start end
    | start < end = start : myRange (start + 1) end
    | start == end = [end]
