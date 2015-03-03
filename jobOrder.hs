import Test.HUnit
import Data.List
import Data.Maybe
import Control.Exception

data Job = Job Char deriving (Show, Eq)

orderJobs :: String -> [Job]
orderJobs = sortJobs . map parseJob . lines

parseJob :: String -> (Job, Maybe Job)
parseJob = createJob . removeWhitespaces
    where removeWhitespaces line = [x | x <- line, x /= ' ']
          createJob (j:'=':'>':[]) = (Job j, Nothing)
          createJob (j:'=':'>':d:[]) = (Job j, Just (Job d))

sortJobs :: [(Job, Maybe Job)] -> [Job]
sortJobs jobs = sort jobs []
    where   sort [] result = reverse result
            sort jobs result = case findNext jobs of
                                    Nothing -> error "cyclic reference!"
                                    Just row@(job, _) -> sort (delete row jobs) (job:result)

findNext :: [(Job, Maybe Job)] -> Maybe (Job, Maybe Job)
findNext jobs = findNext' jobs (map fst jobs)
    where   findNext' (current@(_, Nothing):_) _ = Just current
            findNext' (current@(_, Just dependency):remaining) unordered =
                if elem dependency unordered
                then findNext' remaining unordered
                else Just current
            findNext' [] _ = Nothing

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
    ),
    TestLabel "cyclic referencing jobs" (
        TestCase $ do
            errorRaised <- evaluate (orderJobs "a => b\n b => c \n c => d \n d => b" `seq` False) `catch` handleError
            if errorRaised
                then return ()
                else assertFailure "should throw an exception, but it didn't"
    )
    ]

containsJob :: Char -> [Job] -> Bool
containsJob expected = elem (Job expected)

handleError :: ErrorCall -> IO Bool
handleError _ = return True
