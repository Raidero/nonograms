import System.Console.ANSI 
import Data.List
import Data.List (elemIndex)

------DRAWING-----------------------------------------------

palette :: [Color]
palette = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]
putCharWithColor :: Char -> Color -> IO ()
putCharWithColor x c = do
 setSGR [SetColor Foreground Vivid c]
 putChar x
 setSGR [Reset]
test :: [Color] -> IO ()
test [] = putChar '\n'
test (c:cs) = do 
    putCharWithColor 'X' c
    test cs


drawLine :: [Int] -> IO()
drawLine [] = putChar '\n'
drawLine (x:xs) = case x of
    -1 -> do
        putCharWithColor 'X' Red
        drawLine xs
    0 -> do
        putCharWithColor 'X' White
        drawLine xs
    1 -> do 
        putCharWithColor 'X' Black
        drawLine xs

drawPuzzle :: [[Int]] -> IO()
drawPuzzle [] = putChar '\n'
drawPuzzle [[]] = putChar '\n'
drawPuzzle (x:xs) = do
    drawLine x
    drawPuzzle xs
----------------------DRAWING^^^^^------------------------------------------
----------------------SIMPLE BOXES------------------------------------------

convert :: [String] -> [Int]
convert = map read
data Puzzle = Puzzle {rows :: [[Int]], columns :: [[Int]]} deriving Show

slice start end = take (end - start + 1) . drop start

spacesArray :: Int -> [Int]
spacesArray n = take n (repeat 0)
boxArray :: Int -> [Int]
boxArray n = take n (repeat 1)
emptyArray :: Int -> [Int]
emptyArray n = take n (repeat (-1))

prepareRowPart  :: Int -> Int -> [Int]
prepareRowPart clue diff = do
    if clue > diff
        then (emptyArray diff) ++ (boxArray (clue - diff)) ++ (emptyArray 1)
        else emptyArray (clue + 1)

solveRow :: [Int] -> Int -> [Int]  
solveRow [] n = emptyArray n 
solveRow row n = do
    let cluesWithSpaces = sum row + length row - 1
    let diff = n - cluesWithSpaces
    let result = concat (map (\x -> prepareRowPart x diff) row) ++ emptyArray (diff - 1) 
    if diff == 0 
        then init result
        else result

doAllRows :: [[String]] -> Int -> [[Int]]
doAllRows x k = 
    map (\elem -> solveRow (convert elem) k) x 


-----------------SIMPLE BOXES^^^^^^^^^^^--------------------------------------
-----------------SIMPLE SPACES----UNDERCONSTRUCTION--------------------------------------------

---[FilledRow/Column] -> [CluesforThatRow/Column]
-- simpleSpaces:: [Int] -> [Int] -> [Int]
-- simpleSpaces (x:xs) (y:ys) = 
--     if (elem 0 (take y (x:xs))) then x : simpleSpaces xs (y:ys)
--     else
--         if (elem 1 (take y (x:xs))) then if isFittable 
--     simpleSpaces (drop' (y+1) xs) (ys)


-- generateAllPossibilities :: [Int] -> [Int] ->[[Int]]
-- generateAllPossibilities (x:xs) (y:ys) = 
    

-----------------SIMPLE SPACES^^^^^^^^^^^^^^---------------------------------------

main = do
    f   <- readFile "nonogramP.txt"
    let list = lines f
    let size = (map read (words (list!!0))) :: [Int]  -- tablica zawierająca wartości x i y size[0] to x itd..
    let colValues = (map words (slice 1 (size!!0) list))-- tablica [kolumna][clue], np. mamy w kolumnie 2 clue numer 3, czyli bierzemy colValues[1][2] (bo indeksy od 0)
    let rowValues = (map words (slice (size!!0 + 1) (size!!0 + size!!1) list))
  
    let columnTest =  [[1,0],[2,3]]
    let rowTest = [1,1,0,-1,1,1,0,-1,-1]
    let poppedAll = [[]]
    let poppedq = [[1],[1],[2],[2]]
    let testrows = [["6","1","1"],["5","2","1"]]

    let fromRows = doAllRows rowValues (size!!0)
    let fromColumns = doAllRows colValues (size!!1)
    let rowsfromColumns = collectHeads fromColumns
    let zipped = zipMany fromRows rowsfromColumns


    ---------------TESTING--------------------------------------------------------

    drawPuzzle zipped

    ---------------PRINTS---------------------------------------------------------

    
    print rowValues
    print colValues
    print ("fromRows", fromRows)
    print ("fromColumns", fromColumns)
    print ( "rowsFromColumns", rowsfromColumns)
    print ("zippedTogether", zipped)
    print (extraSpace [3,2,1])
    print (drop' ((position 1 [0,0,0,1,0,1,0])+1) [0,0,0,1,0,1,0])
    print (take 5 [0,0,0,0,0,0])
    print (isFittable [1,0] [2])
    --let row = solveRow [5, 2, 1] 11

    --mapM_ print row 
    --spell (rowValues!!3!!0)
    
    -- print (map (\elemxy -> overwriteMoreImportantValue elemxy) (zip ))

    --print (isFittable [-1,-1,-1,-1,-1,-1,-1,-1] [5,3])
----------------------SUPORTIVE FUNCTIONS----------------------------------------------

---------TODO----
---------- Function that computes extra space needed for list of clues would be handfull (take the color into account)
---------- Simple spaces algorithm
--------------
extraSpace :: [Int] -> Int
extraSpace [x] = x
extraSpace (x:xs) = x + 1 + extraSpace xs

isFittable ::  [Int] -> [Int] -> Bool
isFittable [] [] = True
isFittable [] (y:ys) = False
isFittable [x] [y] = if x==0 || y>1 then False else True
isFittable [x] (y:ys) = if x==0 || y>1 then False else True
isFittable (x:xs) [] = if elem 1 (x:xs) then False else True
isFittable (x:xs) (y:ys) =
    if (length (x:xs) < y) then False else
        if (length (x:xs) == y && not(elem 0 (x:xs))) then True else
            if (elem 0 (take y (x:xs))) then isFittable (drop' ((position 0 (x:xs))+1) (x:xs)) (y:ys) else
                if (head (drop' y (x:xs)) == 1) then isFittable xs (y:ys) else
                True && isFittable (drop' y (x:xs)) (ys)


            -- if (elem 0 (take y (x:xs))) then isFittable (drop' ((position 0 (x:xs))+1) (x:xs)) (y:ys) else
            --     True && (isFittable (drop' (y+1) (x:xs)) ys)
        

position :: Eq a => a -> [a] -> Int
position i xs =
    case i `elemIndex` xs of
       Just n  -> n
       Nothing -> 0

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 ys = ys
drop' x ys = drop' (x-1) (tail ys)

zipMany :: [[Int]] -> [[Int]] -> [[Int]]
zipMany [] [] = []
zipMany (x:xs) (y:ys) = overrideForArray (zip x y) : zipMany xs ys


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
    if x == [] then collectHeads (popAllFirst (xs))
    else listFromHeads (x:xs) : collectHeads (popAllFirst (x:xs))

overrideForArray :: [(Int,Int)] -> [Int]
overrideForArray p = map (\n -> overwriteMoreImportantValue n) p

--needed for merging two part-puzzles from rows and columns
overwriteMoreImportantValue :: (Int,Int) -> Int
overwriteMoreImportantValue (x, y)= 
    if x == -1 then y
    else
    if y == -1 then x
    else x

