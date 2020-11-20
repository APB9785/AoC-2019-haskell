import Data.Function

data Vector = Position Int Int Int | Velocity Int Int Int
data Moon = Moon { posX :: Int
                 , posY :: Int
                 , posZ :: Int
                 , velX :: Int
                 , velY :: Int
                 , velZ :: Int
                 } deriving (Show)

main = do
    contents <- readFile "day12input.txt"

    putStr formatHeader

    let initialState = map parseInput (lines contents)
      
    runSteps 1000 initialState
      & map energyTotal
      & sum
      & print

    putStr "\nPart 2:  "

    -- To find how many steps it takes to repeat the position, we look for
    -- a "cycle" where all velocities returns to 0 along one axis.
    let x_cycle = runInfinite initialState 'x'
        y_cycle = runInfinite initialState 'y'
        z_cycle = runInfinite initialState 'z'

    -- Then we can take the LCM of all three cycles, and multiply by two
    -- To find how long it takes for all three cycles to converge.
    lcm y_cycle z_cycle
      & lcm x_cycle
      & (*) 2
      & print

    putStr "\n"


-- Simple loop counter
runSteps :: Int -> [Moon] -> [Moon]
runSteps 0 moonList = moonList
runSteps i moonList = runSteps (i - 1) (runOneStep moonList)

-- Loop until all velocities are 0 along a given axis
runInfinite :: [Moon] -> Char -> Int
runInfinite moonList axis = go (runOneStep moonList) 1
    where
      go ms count =
        if   axis `velEqZero` ms
        then count
        else go (runOneStep ms) (count + 1)

runOneStep :: [Moon] -> [Moon]
runOneStep [a, b, c, d] =
    let a' = applyGrav a [b, c, d]
        b' = applyGrav b [a, c, d]
        c' = applyGrav c [a, b, d]
        d' = applyGrav d [a, b, c]
    in [(applyVel a'), (applyVel b'), (applyVel c'), (applyVel d')]

applyGrav :: Moon -> [Moon] -> Moon
applyGrav base others =
    let x = (velX base) + (sum $ map (comparePos 'x' base) others)
        y = (velY base) + (sum $ map (comparePos 'y' base) others)
        z = (velZ base) + (sum $ map (comparePos 'z' base) others)
    in  Moon {posX = posX base, posY = posY base, posZ = posZ base,
              velX = x, velY = y, velZ = z}

applyVel :: Moon -> Moon
applyVel m =
    Moon {posX = (posX m) + (velX m),
          posY = (posY m) + (velY m),
          posZ = (posZ m) + (velZ m),
          velX = velX m,
          velY = velY m,
          velZ = velZ m}

velEqZero :: Char -> [Moon] -> Bool
velEqZero axis [a, b, c, d]
    | axis == 'x' = ((velX a) == 0) && ((velX b) == 0) && ((velX c) == 0) && ((velX d) == 0)
    | axis == 'y' = ((velY a) == 0) && ((velY b) == 0) && ((velY c) == 0) && ((velY d) == 0)
    | axis == 'z' = ((velZ a) == 0) && ((velZ b) == 0) && ((velZ c) == 0) && ((velZ d) == 0)

comparePos :: Char -> Moon -> Moon -> Int
comparePos axis a b
    | axis == 'x' =
        if   (posX a) < (posX b)
        then 1
        else (if (posX a) > (posX b) then (-1) else 0)
    | axis == 'y' =
        if (posY a) < (posY b)
        then 1
        else (if (posY a) > (posY b) then (-1) else 0)
    | axis == 'z' =
        if (posZ a) < (posZ b)
        then 1
        else (if (posZ a) > (posZ b) then (-1) else 0)

energyTotal :: Moon -> Int
energyTotal m =
    let pot = (abs $ posX m) + (abs $ posY m) + (abs $ posZ m)
        kin = (abs $ velX m) + (abs $ velY m) + (abs $ velZ m)
    in  pot * kin

parseInput :: String -> Moon
parseInput s =
    let s' = init $ tail s
        s2 = splitCSV s'
        [x, y, z] = map (\(i:'=':rest) -> read rest :: Int) s2
    in Moon {posX = x, posY = y, posZ = z, velX = 0, velY = 0, velZ = 0}

-- Splits on comma+space delimiter
splitCSV :: [Char] -> [String]
splitCSV [] = []
splitCSV x  =
    let (curr, rest) = break (==',') x in
    case rest of
      [] -> curr:(splitCSV rest)
      _  -> curr:(splitCSV $ tail $ tail rest)


formatHeader = "\n***********************\nADVENT OF CODE - DAY 12" ++
    "\n***********************\n\nPart 1:  "
