import Data.List

getLetters :: String -> [Int] -> [Char]
getLetters pass nums = map fst $
                       filter (\(c, n) -> if n `elem` nums then True else False) $ 
                       zip pass [1..(length pass)]

main :: IO ()
main = do
    pass <- putStr "Please enter your password: " >> getLine
    nums <- putStr "please enter the digits of the fields (separated by spaces): " >> map read <$> words <$> getLine
    putStr "letters in order: "
    putStrLn $ intersperse ',' $ getLetters pass (sort nums)