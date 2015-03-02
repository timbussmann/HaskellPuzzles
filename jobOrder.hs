import Test.HUnit
import Data.List
import Control.Exception

data Job = Job Char | DependentJob Char Char deriving (Show)

instance Eq Job where
    x == y = valueOf x == valueOf y
    x /= y = not (x == y)

valueOf :: Job -> Char
valueOf (Job j) = j
valueOf (DependentJob j _) = j

orderJobs :: String -> [Job]
orderJobs input = let jobs = map parseJob (lines input)
                  in sortJobs (verifyNonSelfReferencing jobs) []

parseJob :: String -> Job
parseJob = createJob . removeWhitespaces
    where removeWhitespaces input = [x | x <- input, x /= ' ']
          createJob (j:'=':'>':[]) = Job j
          createJob (j:'=':'>':d:[]) = DependentJob j d

sortJobs :: [Job] -> [Job] -> [Job]
-- the job has no dependency -> add it to the result list
sortJobs (job@(Job j):xs) result = sortJobs xs (job:result)
sortJobs (job@(DependentJob j d):xs) result
    -- the job has a dependency, which is already in the result list -> add the dependency
    | containsJob d result = sortJobs xs (job:result)
    -- the dependency is not in the result list -> re-queue the job at the end of the jobs list
    | otherwise = sortJobs (xs ++ [job]) result
-- since we use the ":" syntax to add jobs to the result, we have to reverse the result
sortJobs [] result = reverse result

verifyNonSelfReferencing :: [Job] -> [Job]
verifyNonSelfReferencing = map verifyJobDependency
    where   verifyJobDependency job@(DependentJob j d) = if j == d then error "job depends on itself!" else job
            verifyJobDependency j = j

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
    TestLabel "single dependency" (
        let result = orderJobs "a => \n b => c\n c =>"
        in TestList [
        TestCase $ assertEqual "contains 3 jobs" 3 (length result),
        TestCase $ assertBool "contains job a" (containsJob 'a' result),
        TestCase $ assertBool "contains job b" (containsJob 'b' result),
        TestCase $ assertBool "contains job c" (containsJob 'c' result),
        TestCase $ assertBool "place dependency before dependent job"
            (findIndex (==(Job 'b')) result > findIndex (==(Job 'c')) result)
        ]
    ),
    TestLabel "multiple dependencies" (
        let result = orderJobs "a => c \n b => a \n c => \n d => b"
        in TestList [
        TestCase $ assertEqual "contains 4 jobs" 4 (length result),
        TestCase $ assertBool "contains job a" (containsJob 'a' result),
        TestCase $ assertBool "contains job b" (containsJob 'b' result),
        TestCase $ assertBool "contains job c" (containsJob 'c' result),
        TestCase $ assertBool "contains job c" (containsJob 'd' result),
        TestCase $ assertBool "place c before a"
            (findIndex (==(Job 'a')) result > findIndex (==(Job 'c')) result),
        TestCase $ assertBool "place a before b"
            (findIndex (==(Job 'b')) result > findIndex (==(Job 'a')) result),
        TestCase $ assertBool "place b before d"
            (findIndex (==(Job 'd')) result > findIndex (==(Job 'b')) result)
        ]
    ),
    TestLabel "self referencing job" (
        TestCase $ do
            errorRaised <- evaluate (orderJobs "a => \n b => b \n c => " `seq` False) `catch` handleError
            if errorRaised
                then return ()
                else assertFailure "should throw an exception, but it didn't"
    ){-,
    TestLabel "cyclic referencing jobs" (
        TestCase $ do
            errorRaised <- evaluate (orderJobs "a => b\n b => c \n c => d \n d => b" `seq` False) `catch` handleError
            if errorRaised
                then return ()
                else assertFailure "should throw an exception, but it didn't"
    )-}
    ]

handleError :: ErrorCall -> IO Bool
handleError _ = return True
