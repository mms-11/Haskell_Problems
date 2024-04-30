fatPrime :: Integer -> [(Integer, Int)]

fatorar :: Integer -> [Integer]
fatorar n
  | n < 2     = []  
  | otherwise = menorFator : fatorar (n `div` menorFator)
  where
    menorFator = head [x | x <- [2..n], n `mod` x == 0]

contarFatoresPrimos :: Integer -> [Integer] -> [(Integer, Int)]
contarFatoresPrimos _ [] = []
contarFatoresPrimos n (x:xs) = (x, count x (x:xs)) : contarFatoresPrimos n (filter (/= x) xs)
    where
        count y = length . filter (== y)

fatPrime n 
    | n < 2 = []
    | otherwise = contarFatoresPrimos n (fatorar n)

main :: IO ()
main = do
      a <- getLine
      let result = fatPrime (read a :: Integer)
      print result

