import Test.HUnit

problem46 = TestList [
    TestCase $ assertEqual "" [
        [True, True, True], 
        [True, False, True], 
        [False, True, False], 
        [False, False, False]] 
            (table (\a b -> (and' a (or' a b))))
    ]

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False
-- simpler: 
--or' False False = False
--or' _     _     = True

table :: (Bool -> Bool -> Bool) -> [[Bool]]
-- table x = [
--     row True True,
--     row True False,
--     row False True,
--     row False False
--     ]
--     where row a b = [a, b, x a b]

-- using list comprehensions:
table f = [[a, b, f a b] | a <- [True, False], b <- [True, False]]