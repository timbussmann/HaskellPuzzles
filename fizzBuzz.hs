main = do
    let result = map fizzBuzz [1..20]
    mapM_ putStrLn result

fizzBuzz :: Int -> String
fizzBuzz x
    | divideableByThree && divideableByFive = "Fizz Buzz"
    | divideableByThree = "Fizz"
    | divideableByFive = "Buzz"
    | otherwise = show x
    where
        divideableByThree = x `mod` 3 == 0
        divideableByFive = x `mod` 5 == 0
