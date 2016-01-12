getLetters :: String -> [Int] -> String
getLetters pass nums = map (\(c, n) -> if n `elem` nums then c else '_') $ 
                       zip pass [1..(length pass)]

main :: IO ()
main = do
    pass <- putStr "Please enter your password: " >> getLine
    nums <- putStr "please enter the nums of fields that will be not empty (separated by spaces): " >> map read <$> words <$> getLine
    putStrLn $ getLetters pass nums