import Test.HUnit
import Data.List

data Job = Job Char | DependentJob Char Char deriving (Show)

instance Eq Job where
    x == y = valueOf x == valueOf y
    x /= y = not (x == y)

valueOf :: Job -> Char
valueOf (Job j) = j
valueOf (DependentJob j _) = j

orderJobs :: String -> [Job]
orderJobs input = let jobs = map parseJob (lines input)
                  in sortJobs jobs []

parseJob :: String -> Job
parseJob line = createJob (removeWhitespaces line)
    where removeWhitespaces input = [x | x <- input, x /= ' ']
          createJob (j:'=':'>':[]) = Job j
          createJob (j:'=':'>':d:[]) = DependentJob j d

sortJobs :: [Job] -> [Job] -> [Job]
sortJobs (job@(Job j):xs) result = sortJobs xs (job:result)
sortJobs (job@(DependentJob j d):xs) result
    | containsJob d result = sortJobs xs (job:result)
    | otherwise = sortJobs (xs ++ [job]) result
sortJobs [] result = reverse result

containsJob :: Char -> [Job] -> Bool
containsJob expected jobs = elem (Job expected) jobs

tests = TestList [
    TestLabel "a single job" (
        let result = orderJobs "a =>"
        in TestList [
        TestCase $ assertEqual "contains single job" 1 (length result),
        TestCase $ assertEqual "contains job a" (Job 'a') (result !! 0)
        ]
    ),
    TestLabel "multiple jobs" (
        let result = orderJobs "a => \n b => \n c =>"
        in TestList [
        TestCase $ assertEqual "contains 3 jobs" 3 (length result),
        TestCase $ assertEqual "contains job a" True (containsJob 'a' result),
        TestCase $ assertEqual "contains job b" True (containsJob 'b' result),
        TestCase $ assertEqual "contains job c" True (containsJob 'c' result)
        ]
    ),
    TestLabel "single dependeny" (
        let result = orderJobs "a => \n b => c\n c =>"
        in TestList [
        TestCase $assertEqual "contains 3 jobs" 3 (length result),
        TestCase $ assertBool "contains job a" (containsJob 'a' result),
        TestCase $ assertBool "contains job b" (containsJob 'b' result),
        TestCase $ assertBool "contains job c" (containsJob 'c' result),
        TestCase $ assertBool "place dependency before dependent job"
            (findIndex (==(Job 'b')) result > findIndex (==(Job 'c')) result)
        ]
    )
    ]
