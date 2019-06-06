spell d = putStrLn d

convert :: [String] -> [Int]
convert = map read
data Puzzle = Puzzle {rows :: [[Int]], columns :: [[Int]]} deriving Show

slice start end = take (end - start + 1) . drop start

main = do
    f   <- readFile "nonogram.txt"
    let list = lines f
    let size = (map read (words (list!!0))) :: [Int]  -- tablica zawierająca wartości x i y size[0] to x itd..
    let colValues = (map words (slice 1 (size!!0) list))-- tablica [kolumna][clue], np. mamy w kolumnie 2 clue numer 3, czyli bierzemy colValues[1][2] (bo indeksy od 0)
    let rowValues = (map words (slice (size!!0 + 1) (size!!0 + size!!1) list))
    print (rowValues!!3!!0)
    let columnTest =  [[1,0],[2,3]]
    let poppedAll = [[]]
    let poppedq = [[1],[1],[2],[2]]
    print (listFromHeads columnTest)
    print (listFromHeads poppedq)
    print (listFromHeads poppedAll)
    print (popFirst (columnTest!!0))
    print (popAllFirst columnTest)
    print (popAllFirst columnTest)
    print (popAllFirst poppedAll)
    print (collectHeads columnTest)
    doAllRows rowValues
    
solveRow :: [Int] -> [Int]
solveRow row = row


doAllRows :: [[String]] -> IO()
doAllRows [] = print ("koniec")
doAllRows (x:xs) = do
    let solvedRow = solveRow (convert x)
    print solvedRow
    doAllRows xs

popFirst :: [Int] -> [Int]
popFirst [] = []
popFirst [x] = []
popFirst (x:xs) = xs

popAllFirst :: [[Int]] -> [[Int]]
popAllFirst [] = []
popAllFirst [[]] = [[]]
popAllFirst (x:xs) = popFirst x : popAllFirst xs



--needed for changing columns to rows
listFromHeads :: [[Int]] -> [Int]
listFromHeads [] = []
listFromHeads [[]] = []
listFromHeads [(x:xs)] = [x]
listFromHeads ((x:xs):xss) = x: listFromHeads xss
listFromHeads (x:xs) = (listFromHeads xs)

collectHeads :: [[Int]] -> [[Int]]
collectHeads [] = []
collectHeads [[]] = []
collectHeads (x:xs) = 
    if x == [] then listFromHeads (xs) : collectHeads (popAllFirst (xs))
    else listFromHeads (x:xs) : collectHeads (popAllFirst (x:xs))