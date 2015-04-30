-- https://wiki.haskell.org/99_questions/31_to_41

import Test.Hspec

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
