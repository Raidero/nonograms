

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
    concat (map (\x -> prepareRowPart x diff) row) ++ emptyArray (diff - 1) 

main = do
    f   <- readFile "nonogram.txt"
    -- let list = lines f
    -- let size = (map read (words (list!!0))) :: [Int]  -- tablica zawierająca wartości x i y size[0] to x itd..
    -- let colValues = map words (slice 1 (size!!0) list) -- tablica [kolumna][clue], np. mamy w kolumnie 2 clue numer 3, czyli bierzemy colValues[1][2] (bo indeksy od 0)
    -- let rowValues = map words (slice (size!!0 + 1) (size!!0 + size!!1) list)
    -- --let result = sp;v
    let row = solveRow [5, 2, 1] 11

    mapM_ print row 
    --spell (rowValues!!3!!0)
    
   