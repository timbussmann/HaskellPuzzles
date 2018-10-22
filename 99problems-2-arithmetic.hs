-- https://wiki.haskell.org/99_questions/31_to_41

import Test.Hspec
import Test.HUnit

problem31 = hspec $ do
    describe "isPrime" $ do
        it "is true when number is a prime" $
            isPrime 7 `shouldBe` True
        it "is false when number is no prime" $
            isPrime 42 `shouldBe` False
        it "is false for 1" $
            isPrime 1 `shouldBe` False
        it "is true for 2" $
            isPrime 2 `shouldBe` True

-- very simple and inefficient implementation
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = not $ any (\x -> n `mod` x == 0) [2..(floor . sqrt . fromIntegral) n]


problem32 = TestList [
    TestCase (assertEqual "1" 9 $ myGCD 63 36),
    TestCase (assertEqual "2" 9 $ myGCD 36 63),
    TestCase (assertEqual "3" 3  $ myGCD (-3) (-6)),
    TestCase (assertEqual "4" 3 $ myGCD (-3) 6)
    ]

myGCD :: Int -> Int -> Int
myGCD x y 
        | x == 0 = y
        | x < 0 = myGCD (abs x) y
        | y < 0 = myGCD x (- y)
        | x == y = x
        | x > y = myGCD (x `mod` y) y
        | x < y = myGCD (y `mod` x) x


problem33 = TestList [
    TestCase (assertEqual "" True $ myCoprime 35 64),
    TestCase (assertEqual "" False $ myCoprime 4 6)
    ]

myCoprime x y = (myGCD x y) == 1