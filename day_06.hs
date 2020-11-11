import Data.Function
import qualified Data.List as List
import qualified Data.HashMap.Strict as Map

main = do
    contents <- readFile "day6input.txt"

    putStr formatHeader

    let orbitMap = List.foldl' mapRelations Map.empty (lines contents)

    countAllOrbits orbitMap
      & sum
      & print

    putStr "\nPart 2:  "

    transfersReqd orbitMap
      & print

    putStr "\n"


mapRelations :: Map.HashMap String String -> [Char] -> Map.HashMap String String
mapRelations acc [a,b,c,d,e,f,g] =
    Map.insert (e:f:[g]) (a:b:[c]) acc

countAllOrbits :: Map.HashMap String String -> [Int]
countAllOrbits m = go (countSinglePath) (Map.keys m) []
    where
      go _ []     done = done
      go f (x:xs) done = go (f) (xs) ((f x m 0):done)

countSinglePath :: String -> Map.HashMap String String -> Int -> Int
countSinglePath "COM"    _        count = count
countSinglePath startVal orbitMap count =
    countSinglePath (orbitMap Map.! startVal) orbitMap (count + 1)

listSinglePath :: String -> Map.HashMap String String -> Map.HashMap String Int
listSinglePath x m = go x m Map.empty
    where
      go "COM" _        done = done
      go name  orbitMap done =
        let next = orbitMap Map.! name in
        go next orbitMap (Map.insert next (Map.size done) done)   

transfersReqd :: Map.HashMap String String -> Int
transfersReqd m = go (m Map.! "YOU") 0
    where
      santaPath = listSinglePath "SAN" m
      go currentLocation distanceTraveled =
        if   currentLocation `Map.member` santaPath
        then distanceTraveled + (santaPath Map.! currentLocation)
        else go (m Map.! currentLocation) (distanceTraveled + 1)
    
formatHeader = "\n**********************\nADVENT OF CODE - DAY 6" ++
    "\n**********************\n\nPart 1:  "
