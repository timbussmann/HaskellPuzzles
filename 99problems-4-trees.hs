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

symmetric :: Tree Char -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror r1 l2 -- ignore values for now
mirror _ _ = False