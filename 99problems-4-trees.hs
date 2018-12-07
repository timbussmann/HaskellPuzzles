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