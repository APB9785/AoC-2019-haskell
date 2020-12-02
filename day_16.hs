import Data.Function
import Data.Char
import qualified Data.List as List

main = do
    contents <- readFile "day16input.txt"

    let initList = map digitToInt (init contents)

    putStr formatHeader

    print $ part1 initList

    putStr "\nPart 2:  "

    print $ part2 initList

    putStr "\n"

-- PART 1

part1 :: [Int] -> Int
part1 ns =
    go ns 100
    where
      go xs 0 = intListToInt (take 8 xs)
      go xs n = go (runPhase xs) (n - 1)

runPhase :: [Int] -> [Int]
runPhase ns = map (calculateElem ns) [1..(length ns)]

calculateElem :: [Int] -> Int -> Int
calculateElem l n =
    go pattern l 0
    where
      go []     []     total = (abs total) `mod` 10
      go (p:ps) (x:xs) total = go ps xs (total + (p * x))

      pattern =
        (cycle ((replicate n 0) ++ (replicate n 1) ++
          (replicate n 0) ++ (replicate n (-1))))
          & take ((length l) + 1)
          & tail

-- PART 2

part2 :: [Int] -> Int
part2 ns =
    take total_size (cycle ns)
      & drop offset
      & runPhase2
      & take 8
      & intListToInt
    where
      offset = intListToInt (take 7 ns)
      total_size = (length ns) * 10000

runPhase2 :: [Int] -> [Int]
runPhase2 ns =
    go ns 100
    where
      go xs 0 = xs
      go xs i =
        let xs' = List.scanr (\x acc -> (x + acc) `mod` 10) 0 xs
        in  go xs' (i - 1)

-- EXTRAS

intListToInt :: [Int] -> Int
intListToInt xs =
    List.foldl' (\acc x -> (acc * 10) + x) 0 xs

formatHeader = "\n***********************\nADVENT OF CODE - DAY 16\n" ++
    "***********************\n\nPart 1:  "
