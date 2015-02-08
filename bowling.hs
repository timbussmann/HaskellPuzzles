import Test.HUnit

data Frame = Open Int Int | Spare Int Int | Strike deriving (Show)

calcScore :: [Int] -> Int
calcScore = sum . take 10 . calculateScore . parseFrames

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

tests = TestList [
    TestCase $ assertEqual "perfect game" 300 (calcScore [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]),
    TestCase $ assertEqual "only open frames" 90 (calcScore [9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0]),
    TestCase $ assertEqual "only spares" 150 (calcScore [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5])
    ]

main = do
    runTestTT tests
