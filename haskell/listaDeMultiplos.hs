produto :: Int -> Int ->Int ->Int
produto a b limite | a*b <= limite = a*b + produto (a+1) b limite
                   | otherwise = 0



somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos [] g  = []



somarMultiplos (x:xs) m 
                          | m > x = 0 : somarMultiplos xs m
                          |m == 0 = 0 : somarMultiplos xs m
                          | otherwise = multiplos x m :  somarMultiplos xs m
                          where multiplos x m = produto 1 m x

main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result