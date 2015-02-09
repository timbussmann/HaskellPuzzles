-- https://wiki.haskell.org/99_questions/1_to_10

import Test.HUnit

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
