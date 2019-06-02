spell d = putStrLn d

slice start end = take (end - start + 1) . drop start

main = do
    f   <- readFile "a.txt"
    let list = lines f
    let size = (map read (words (list!!0))) :: [Int]  -- tablica zawierająca wartości x i y size[0] to x itd..
    let colValues = map words (slice 1 (size!!0) list) -- tablica [kolumna][clue], np. mamy w kolumnie 2 clue numer 3, czyli bierzemy colValues[1][2] (bo indeksy od 0)
    let rowValues = map words (slice (size!!0 + 1) (size!!0 + size!!1) list)
    spell (rowValues!!3!!1)
    
   