import Data.Function

main = do
    contents <- readFile "input.txt"

    putStr outputHeader

    -- PART 1

    lines contents
      & map (\n -> fuelReqOne $ read n :: Int)
      & sum
      & print
    
    -- PART 2

    putStr "\n\nPart 2:  "

    lines contents
      & map (\n -> fuelReqTwo $ read n :: Int)
      & sum
      & print

    putStr "\n"


fuelReqOne :: Int -> Int
fuelReqOne n = (n `div` 3) - 2

fuelReqTwo :: Int -> Int
fuelReqTwo n 
     | n `div` 3 - 2 >= 0 = n `div` 3 - 2 + (fuelReqTwo $ n `div` 3 - 2)
     | n `div` 3 - 2 < 0  = 0

outputHeader = "\n***********************\nADVENT OF CODE - DAY 1" ++
    "\n***********************\n\nPart 1:  "
