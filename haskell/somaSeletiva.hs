import Data.Char (isDigit)
import Data.Char (digitToInt)
sumNumbers :: String -> Int
sumNumbers [] = 0
sumNumbers (x:xs) | isDigit x = digitToInt x + sumNumbers xs
                  | otherwise = sumNumbers xs

main :: IO ()
main = do
  a <- getLine
  let result = sumNumbers a
  print result