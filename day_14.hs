import Text.Regex.TDFA
import Data.Function
import qualified Data.HashMap.Strict as Hash
import qualified Data.List as List

data Recipe = Recipe { result :: Int
                     , reqs :: [(Int, String)]
                     } deriving (Show)

data LabState = LabState { book :: Hash.HashMap String Recipe
                         , inv  :: Hash.HashMap String Int
                         } deriving (Show)

main = do
    contents <- readFile "day14input.txt"

    -- Recipe book of conversion fomulas
    let book1 = lines contents
                  & map parseLine
                  & List.foldl' makeHash Hash.empty

    -- Inventory of all known reagents.  Starts with all zeroes.
    let inv1  = List.foldl' (\acc x -> Hash.insert x 0 acc)
                            (Hash.singleton "ORE" 0) (Hash.keys book1)

    putStr formatHeader

    -- Amount of ORE required to make the first FUEL from scratch.
    let base = LabState {book = book1, inv = Hash.insert "FUEL" (-1) inv1}
                 & orePerFuel
                 & (\x -> 0 - (x Hash.! "ORE"))

    print base

    putStr "\nPart 2:  "

    let oreGiven = 1000000000000

    -- Maximum possible FUEL created with the given amount of ORE.
    findMaxFuel LabState {book = book1, inv = inv1} base oreGiven
      & print

    putStr "\n"


-- Binary search for the right amount of fuel to maximize ore usage
findMaxFuel :: LabState -> Int -> Int -> Int
findMaxFuel state base startOre =
    go state
       (truncate ((realToFrac startOre) / (realToFrac base)))
       startOre
    where
      go s l r
        | r == l + 1 = l
        | otherwise  =
            let m   = floor ((realToFrac (l + r)) / (realToFrac 2))
                ore = (Hash.!)
                        (LabState {book = book s,
                                   inv  = Hash.insert "FUEL" (0 - m) (inv s)}
                           & orePerFuel)
                        ("ORE")
            in  if   ore < (0 - startOre)
                then go s l m     -- too high
                else go s m r     -- loo low

-- Loops through the transmutation process until all regents have positive
-- quantities except for ORE (which cannot be created by transmutation).
orePerFuel :: LabState -> Hash.HashMap String Int
orePerFuel state =
    if   onlyOre $ inv state
    then inv state
    else orePerFuel $ List.foldl' transmute state (Hash.keys $ inv state)

-- Checks that all quantities are positive except ORE.
onlyOre :: Hash.HashMap String Int -> Bool
onlyOre m = go m (Hash.keys m)
    where
      go m []         = True
      go m ("ORE":ks) = go m ks
      go m (k:ks)     = if   m Hash.! k < 0
                        then False
                        else go m ks

-- Checks each type of reagent (except ORE) and creates more if
-- the quantity is negative.
transmute :: LabState -> String -> LabState
transmute state "ORE" = state
transmute state item  =
    let qtyHeld = (inv state) Hash.! item
    in  if   qtyHeld < 0
        then let recipeX = (book state) Hash.! item
                 mplier  = ceiling ((realToFrac (0 - qtyHeld)) /
                                    (realToFrac (result recipeX)))
                 reqList = reqs recipeX
                 newInv  =
                   Hash.update (\x -> Just (x + (mplier*(result recipeX))))
                               item (inv state)
             in  LabState {book = book state,
                           inv  = List.foldl' (subReqs mplier) newInv reqList}
        else state
        where
          subReqs mplier acc (amt, name) =
            Hash.update (\x -> Just (x - (mplier*amt))) name acc

-- Uses Regex to parse one line of input
parseLine :: [Char] -> ([(Int, String)], (Int, String))
parseLine l =
        -- Split on " => " to separate ingredients from the resulting product
    let (_, _, _, [ing, res]) = l =~ "([0-9, A-Z]+) => ([0-9 A-Z]+)" ::
                                     (String, String, String, [String])
        -- Split each ingredient into its own List
        b = ing =~ "([0-9]+) ([A-Z]+)" :: [[String]]
        -- Parses each ingredient List into quantity and name Tuple
        b' = map (\[_, n, x] -> ((read n :: Int), x)) b
        -- Parses resulting product into quantity and name variables
        (_, _, _, [i, j]) = res =~ "([0-9]+) ([A-Z]+)" ::
                                   (String, String, String, [String])

    in  (b', ((read i :: Int), j))

-- Takes one parsed line and adds it to a HashMap of Recipes
makeHash ::
  Hash.HashMap String Recipe
  -> ([(Int, String)], (Int, String))
  -> Hash.HashMap String Recipe
makeHash acc (xs, (n, s)) =
    Hash.insert s Recipe {result = n, reqs = xs} acc

formatHeader = "\n***********************\nADVENT OF CODE - DAY 14" ++
    "\n***********************\n\nPart 1:  "
