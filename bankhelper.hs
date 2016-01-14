import Data.List

main :: IO ()
main = do
    pass <- putStr "Please enter your password: " >> getLine
    nums <- putStr "please enter the digits of the fields: " >> map read <$> words <$> getLine
    let groups = foldr getSequences [] $
                 -- so that there is no out of bounds exception
                 map (\n -> n-1) nums where
        getSequences new [] = [[new]]
        getSequences new (a:as)
            | head a - new <= 1 = (new:a):as
            | otherwise         = [new]:a:as
    let letters = map (\sublist -> map (\n -> pass !! n) sublist) groups
    putStrLn $ show letters