import Test.HUnit

tests = TestList [
    TestCase $ assertEqual "really bad game" 0 (calcScore [ 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0]),
    TestCase $ assertEqual "perfect game" 300 (calcScore [ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]),
    TestCase $ assertEqual "only open frames" 90 (calcScore [ 9,0, 9,0, 9,0, 9,0, 9,0, 9,0, 9,0, 9,0, 9,0, 9,0]),
    TestCase $ assertEqual "only spares" 150 (calcScore [ 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5]),
    TestCase $ assertEqual "10th frame strike" 11 (calcScore [ 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 10, 1, 0]),
    TestCase $ assertEqual "9th frame strike" 12 (calcScore [ 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 10, 1,0]),
    TestCase $ assertEqual "10th frame spare" 32 (calcScore [ 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 10, 0,10, 2])
    ]

main = do
    runTestTT tests

calcScore :: [Int] -> Int
--calcScore = sum . take 10 . calculateScore . parseFrames
calcScore = sum . take 10 . calcFrameScores

-- ################## 2nd approach: ##################

calcFrameScores :: [Int] -> [Int]
calcFrameScores (t1 : t2 : t3 : ts)
    | t1 == 10 = 10 + t2 + t3 : calcFrameScores (t2 : t3 : ts)
    | t1 + t2 == 10 = 10 + t3 : calcFrameScores (t3 : ts)
calcFrameScores (t1 : t2 : ts) = t1 + t2 : calcFrameScores ts
--calcFrameScores _ = [] -- this line is not necessary thanks to haskell's lazy evaluation

-- ################## 1st approach: ##################

data Frame = Open Int Int | Spare Int Int | Strike deriving (Show)

parseFrames :: [Int] -> [Frame]
parseFrames [] = []
parseFrames (t : ts)
    | t == 10 = Strike : (parseFrames ts)
parseFrames (t1 : t2 : ts)
    | t1 + t2 >= 10 = (Spare t1 t2) : (parseFrames ts)
    | otherwise = (Open t1 t2) : (parseFrames ts)
parseFrames (t1 : []) = [Open t1 0]

calculateScore :: [Frame] -> [Int]
calculateScore [] = []
calculateScore (Open t1 t2 : fs) = t1 + t2 : calculateScore fs
calculateScore (Spare _ _ : fs) = 10 + (calcBonus 1 fs) : calculateScore fs
calculateScore (Strike : fs) = 10 + (calcBonus 2 fs) : calculateScore fs

calcBonus :: Int -> [Frame] -> Int
calcBonus 2 (Strike : fs) = 10 + (calcBonus 1 fs)
calcBonus 1 (Strike : _) = 10
calcBonus 2 (Spare t1 t2 : _) = t1 + t2
calcBonus 1 (Spare t1 _ : _) = t1
calcBonus 2 (Open t1 t2 : _) = t1 + t2
calcBonus 1 (Open t1 _ : _) = t1
calcBonus _ [] = 0
