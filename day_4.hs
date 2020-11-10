import Data.Function

-- Given in puzzle input
inputRange = [146810..612564]

main = do

    putStr formatHeader

    -- PART 1

    filter partOneConditionsMet inputRange
      & length
      & print

    -- PART 2

    putStr "\nPart 2:  "

    filter partTwoConditionsMet inputRange
      & length
      & print

    putStr "\n"


partOneConditionsMet :: Int -> Bool
partOneConditionsMet n =
    let x = show n in
    (containsAdjacentDigits x) && (digitsNeverDecrease x)

partTwoConditionsMet :: Int -> Bool
partTwoConditionsMet n =
    let x = show n in
    (digitsNeverDecrease x) && (strictlyTwoAdjacentDigits x)

containsAdjacentDigits :: [Char] -> Bool
containsAdjacentDigits n = go n
    where
    go [_] = False
    go (x:y:rest)
      | x == y    = True
      | otherwise = go (y:rest)

digitsNeverDecrease :: [Char] -> Bool
digitsNeverDecrease n = go n
    where
    go [_] = True
    go (x:y:rest)
      | (read [x] :: Int) > (read [y] :: Int) = False
      | otherwise                             = go (y:rest)

formatHeader = "\n***********************\nADVENT OF CODE - DAY 4" ++
    "\n***********************\n\nPart 1:  "

strictlyTwoAdjacentDigits :: [Char] -> Bool
strictlyTwoAdjacentDigits n =
    2 `elem` (countConsecutive n)

countConsecutive :: [Char] -> [Int]
countConsecutive (x:xs) = go xs 1 x []
    where
    go [] count _ done = count:done
    go (y:ys) count hold done
      | y == hold  = go ys (count+1) hold done
      | y /= hold  = go ys 1         y    (count:done)
