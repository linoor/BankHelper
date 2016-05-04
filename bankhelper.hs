import Control.Applicative

main :: IO ()
main = do
  putStrLn "your pass?"
  pass <- getLine
  putStrLn "blanks?"
  blanks <- map (\x -> x-1) <$> map read <$> words <$> getLine
  putStrLn $ clean pass blanks


clean :: String -> [Int] -> String
clean letters blanks = clean' letters blanks 0 where
  clean' _ [] _          = ""
  clean' [] _ _          = ""
  clean' (firstLetter:restLetters) blanks'@(firstBlank:restBlanks) iter
    | iter == firstBlank = firstLetter : clean' restLetters restBlanks (iter+1)
    | otherwise          = " " ++ clean' restLetters blanks' (iter+1)