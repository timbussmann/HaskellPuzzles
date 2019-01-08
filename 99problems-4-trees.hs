import Test.HUnit

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

problem55 = TestCase $ assertEqual "" [
    Branch 'x' (Branch 'x' Empty Empty) 
               (Branch 'x' Empty 
                           (Branch 'x' Empty Empty)),

    Branch 'x' (Branch 'x' Empty Empty) 
               (Branch 'x' (Branch 'x' Empty Empty) 
                           Empty),
     
    Branch 'x' (Branch 'x' Empty 
                           (Branch 'x' Empty Empty)) 
               (Branch 'x' Empty Empty),
     
    Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
                           Empty) 
               (Branch 'x' Empty Empty)
    ] (cbalTree 4)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `divMod` 2 in [Branch 'x' left right | i <- [q .. (q + r)],
                                                                   left <- cbalTree i,
                                                                   right <- cbalTree (n - i - 1)]


problem56 = TestList [
    TestCase $ assertEqual "when tree is not symmetric" False (symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)),
    TestCase $ assertEqual "when tree is symmetric" True (symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))),
    TestCase $ assertEqual "when complex tree is symmetric" True (symmetric (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty  (Branch 'x' Empty Empty))))
    ]

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror r1 l2 -- ignore values
mirror _ _ = False


problem57 = TestList [
    TestCase $ assertEqual "construction" (Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))) (construct [3,2,5,7,1]),
    let l = construct [5,3,18,1,4,12,21]
    in TestCase $ assertEqual "symmetric construction" True (symmetric l),
    let l = construct [3,2,5,7,4]
    in TestCase $ assertEqual "asymmetric construction" False (symmetric l)
    ]

construct :: (Ord a) => [a] -> Tree a
construct xs = foldl (flip add) Empty xs

add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x (Branch y l r)
    | x > y = Branch y l (add x r)
    | x < y = Branch y (add x l) r

tree4 = Branch 1 
            (Branch 2 
                Empty 
                (Branch 4 Empty Empty)) 
            (Branch 2 Empty Empty)

problem61 = TestList [
    TestCase $ assertEqual "" 0 (countLeaves Empty),
    TestCase $ assertEqual "" 2 (countLeaves tree4)
    ]

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r