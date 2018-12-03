import Test.HUnit

problem46 = TestCase $ assertEqual "" [
    [True, True, True], 
    [True, False, True], 
    [False, True, False], 
    [False, False, False]] 
        (table (\a b -> (and' a (or' a b))))

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


problem47 = TestCase $ assertEqual "" [
    [True, True, True], 
    [True, False, True], 
    [False, True, False], 
    [False, False, False]] 
        (table (\a b -> a `and'` (a `or'` not b)))


problem48 = TestCase $ 
    assertEqual "" ["000","001","010","011","100","101","110","111"] (gray 3)

gray :: Int -> [String]
gray 0 = [""]
gray n = let prev = gray (n - 1) in map ('0':) prev ++ map ('1':) prev
-- gray n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray (n-1)