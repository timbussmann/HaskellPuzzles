import Test.HUnit
import Data.List
import Control.Exception

data Job = Job Char deriving (Show, Eq)

orderJobs :: String -> [Job]
orderJobs input = let jobs = map parseJob (lines input)
                  in sortJobs jobs []

parseJob :: String -> (Job, Maybe Job)
parseJob = createJob . removeWhitespaces
    where removeWhitespaces input = [x | x <- input, x /= ' ']
          createJob (j:'=':'>':[]) = (Job j, Nothing)
          createJob (j:'=':'>':d:[]) = (Job j, Just (Job d))

sortJobs :: [(Job, Maybe Job)] -> [Job] -> [Job]
sortJobs ((job, Nothing):remaining) result = sortJobs remaining (job:result)
sortJobs (entry@(job, Just dependency):remaining) result
    | job == dependency = error "self referencing job!"
    | dependency `elem` result = sortJobs remaining (job:result)
    | otherwise = sortJobs (remaining ++ [entry]) result
sortJobs [] result = reverse result

containsJob :: Char -> [Job] -> Bool
containsJob expected = elem (Job expected)

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
