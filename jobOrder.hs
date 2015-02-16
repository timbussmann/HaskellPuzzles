import Test.HUnit

data Job = Job Char deriving (Show, Eq)

orderJobs :: String -> [Job]
orderJobs input = map parseJob (lines input)

parseJob :: String -> Job
parseJob line = Job $ head (removeWhitespaces line)
    where removeWhitespaces input = [x | x <- input, x /= ' ']

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
        TestCase $ assertEqual "contains job a" True (elem (Job 'a') result),
        TestCase $ assertEqual "contains job b" True (elem (Job 'b') result),
        TestCase $ assertEqual "contains job c" True (elem (Job 'c') result)
        ]
    )
    ]
-- todo: single dependency
