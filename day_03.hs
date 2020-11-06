import Data.Function
import Data.Text (Text, pack, unpack, split, strip)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

main = do
    contents <- readFile "input.txt"

    putStr formatOutputHeader
    
    let (a, b) = parseInput contents

    -- PART 1

    let stateA1 = makePath a
    let stateB1 = makePath b

    HashSet.filter (\n -> HashSet.member n stateA1) stateB1
      & HashSet.map sumTuple
      & minimum
      & print

    -- PART 2

    putStr "\nPart 2:  "

    let stateA2 = makePathTwo a
    let stateB2 = makePathTwo b

    HashMap.filterWithKey (\k v -> k `HashMap.member` stateA2) stateB2
      & HashMap.mapWithKey (\k v -> v + (stateA2 HashMap.! k))
      & HashMap.elems
      & minimum
      & print

    putStr "\n"

-- Uses Data.Text to split the input
parseInput :: String -> ([(Char, [Char])], [(Char, [Char])])
parseInput i = 
    let [a, b] = lines i & map pack & map (split (==','))
        parsedA = map unpack a & map parseSingle
        parsedB = map unpack b & map parseSingle in
    (parsedA, parsedB)
        where 
        parseSingle :: [Char] -> (Char, [Char])
        parseSingle (x:xs) = (x, xs)

-- Lists the coordinates traveled by the wire.
makePath :: [(Char, [Char])] -> HashSet.HashSet (Int, Int)
makePath n = go 0 0 n HashSet.empty
    where 
    go _ _ [] done = done
    go x y ((direction, steps):t) done =
        let (x', y', d') = travel x y direction (read steps :: Int) in
        go x' y' t (d' `HashSet.union` done)

-- Part 2 version of makePath.
makePathTwo :: [(Char, [Char])] -> HashMap.HashMap (Int, Int) Int
makePathTwo n = go 0 0 n HashMap.empty 0
    where
    go _ _ [] done _ = done
    go x y ((direction, steps):rest) done timer =
        let (x', y', d', t') = travelTwo x y direction (read steps :: Int) timer in
        go x' y' rest (done `HashMap.union` d') t'

-- Helper for makePath.  Fully travels one direction at a time.
travel :: Int -> Int -> Char -> Int -> (Int, Int, HashSet.HashSet (Int, Int))
travel x y d s
    | d == 'U' = goUp x y s HashSet.empty
    | d == 'D' = goDown x y s HashSet.empty
    | d == 'R' = goRight x y s HashSet.empty
    | d == 'L' = goLeft x y s HashSet.empty
        where
        goUp x y 0 done = (x, y, done)
        goUp x y s done = goUp x (y+1) (s-1) (HashSet.insert (x, y+1) done)
        goDown x y 0 done = (x, y, done)
        goDown x y s done = goDown x (y-1) (s-1) (HashSet.insert (x, y-1) done)
        goRight x y 0 done = (x, y, done)
        goRight x y s done = goRight (x+1) y (s-1) (HashSet.insert (x+1, y) done)
        goLeft x y 0 done = (x, y, done)
        goLeft x y s done = goLeft (x-1) y (s-1) (HashSet.insert (x-1, y) done)

-- Part 2 version of travel.
travelTwo :: Int -> Int -> Char -> Int -> Int -> (Int, Int, HashMap.HashMap (Int, Int) Int, Int)
travelTwo x y d s t
    | d == 'U' = goUp x y s HashMap.empty t
    | d == 'D' = goDown x y s HashMap.empty t
    | d == 'R' = goRight x y s HashMap.empty t
    | d == 'L' = goLeft x y s HashMap.empty t
        where
        goUp x y 0 done t = (x, y, done, t)
        goUp x y s done t = goUp x (y+1) (s-1) (HashMap.insertWith keepOldValue (x, y+1) (t+1) done) (t+1)
        goDown x y 0 done t = (x, y, done, t)
        goDown x y s done t = goDown x (y-1) (s-1) (HashMap.insertWith keepOldValue (x, y-1) (t+1) done) (t+1)
        goRight x y 0 done t = (x, y, done, t)
        goRight x y s done t = goRight (x+1) y (s-1) (HashMap.insertWith keepOldValue (x+1, y) (t+1) done) (t+1)
        goLeft x y 0 done t = (x, y, done, t)
        goLeft x y s done t = goLeft (x-1) y (s-1) (HashMap.insertWith keepOldValue (x-1, y) (t+1) done) (t+1)
        keepOldValue new old = old

sumTuple :: (Int, Int) -> Int
sumTuple (x, y) = (abs x) + (abs y)

extractThird :: (Int, Int, Int) -> Int
extractThird (x, y, t) = t

formatOutputHeader = "\n***********************\nADVENT OF CODE -- DAY 3\n" ++
    "***********************\n\nPart 1:  "
