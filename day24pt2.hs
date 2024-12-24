-- This one's a bit fun:
-- The alg here doesn't work for every input,
-- and I didn't even use it for my solve.
-- For my actual solve, I just manually figured
-- out swaps using detectiveTilFixpoint.

import System.IO
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Text.Printf (printf)
import Data.Bifunctor (Bifunctor(bimap))

data Logic =
    Input String |
    And Logic Logic |
    Or Logic Logic |
    Xor Logic Logic
    deriving (Show)

toPair [k, _] = (k, Input k)

a ⊻ b = (a || b) && not (a && b)

toGate [a, "OR", b, "->", out] = (a, Or, b, out)
toGate [a, "AND", b, "->", out] = (a, And, b, out)
toGate [a, "XOR", b, "->", out] = (a, Xor, b, out)

toGate2 [a, op, b, "->", out] = (out, (op, a, b))

toSet [a, _, b, "->", out] = [a, b, out]

unwrap (Just a) = a

applyGate input [] = input
applyGate input ((a, op, b, out):xs)
    | HM.member out input = applyGate input xs
    | HM.member a input && HM.member b input = do
        let aval = unwrap (HM.lookup a input)
        let bval = unwrap (HM.lookup b input)
        let newInputs = HM.insert out (aval `op` bval) input
        applyGate newInputs xs
    | otherwise = applyGate input xs

applyTilFixed targ gates input
    | targ == HM.keysSet input = input
    | otherwise = applyTilFixed targ gates (applyGate input gates)

isOkay (Input _) = True
isOkay (Xor (Input _) (Input _)) = True
isOkay (Xor (Xor _ _) (Input _)) = True
isOkay (Xor (Input _) (Xor _ _)) = True
isOkay (Xor (Xor _ _) (And _ _)) = True
isOkay (Xor (And _ _) (Xor _ _)) = True
isOkay (And (Xor _ _) (Input _)) = True
isOkay (And (Input _) (Xor _ _)) = True
isOkay (And (Xor _ _) (Or  _ _)) = True
isOkay (And (Or  _ _) (Xor _ _)) = True
isOkay (And (Or  _ _) (Input _)) = True
isOkay (And (Input _) (Or  _ _)) = True
isOkay (And (Input _) (Input _)) = True
isOkay (Or  (And _ _) (And _ _)) = True
isOkay _ = False

toPaired (a, op, b, out) = (out, [a, b])

data DetectiveResult =
    JustInput String |
    OutputBit String |
    CarrryBit String |
    XorInterm String |
    CryInterm String |
    ProbablyWrong DetectiveInput
    deriving (Show, Eq)

data DetectiveInput =
    LiteralIn String |
    LiteralAnd DetectiveResult DetectiveResult |
    LiteralOr DetectiveResult DetectiveResult |
    LiteralXor DetectiveResult DetectiveResult
    deriving (Show, Eq)

detective (LiteralIn v) = Just (JustInput v)
detective (LiteralXor (JustInput "x00") (JustInput "y00")) = Just (OutputBit "z00")
detective (LiteralXor (JustInput "y00") (JustInput "x00")) = Just (OutputBit "z00")
detective (LiteralAnd (JustInput "x00") (JustInput "y00")) = Just (CarrryBit "z00")
detective (LiteralAnd (JustInput "y00") (JustInput "x00")) = Just (CarrryBit "z00")
detective (LiteralOr (CryInterm v1) (CryInterm v2)) = if v1 == v2 then Just (CarrryBit v1) else Nothing
detective (LiteralXor (JustInput v1) (JustInput v2)) = if tail v1 == tail v2 then Just (XorInterm ('z':tail v1)) else Nothing
detective (LiteralXor (CarrryBit v1) (JustInput v2)) = if read (tail v1) + 1 == read (tail v2) then Just (XorInterm ('z':tail v2)) else Nothing
detective (LiteralXor (JustInput v2) (CarrryBit v1)) = if read (tail v1) + 1 == read (tail v2) then Just (XorInterm ('z':tail v2)) else Nothing
detective (LiteralXor (XorInterm v1) (JustInput v2)) = if tail v1 == tail v2 then Just (OutputBit ('z':tail v1)) else Nothing
detective (LiteralXor (JustInput v1) (XorInterm v2)) = if tail v1 == tail v2 then Just (OutputBit ('z':tail v1)) else Nothing
detective (LiteralXor (CarrryBit v1) (XorInterm v2)) = if read (tail v1) + 1 == read (tail v2) then Just (OutputBit ('z':tail v2)) else Nothing
detective (LiteralXor (XorInterm v2) (CarrryBit v1)) = if read (tail v1) + 1 == read (tail v2) then Just (OutputBit ('z':tail v2)) else Nothing
detective (LiteralAnd (CarrryBit v1) (XorInterm v2)) = if read (tail v1) + 1 == read (tail v2) then Just (CryInterm ('z':tail v2)) else Nothing
detective (LiteralAnd (XorInterm v2) (CarrryBit v1)) = if read (tail v1) + 1 == read (tail v2) then Just (CryInterm ('z':tail v2)) else Nothing
detective (LiteralAnd (JustInput v1) (JustInput v2)) = if tail v1 == tail v2 then Just (CryInterm ('z':tail v1)) else Nothing
detective (LiteralAnd (CarrryBit v1) (JustInput v2)) = if read (tail v1) + 1 == read (tail v2) then Just (CryInterm ('z':tail v2)) else Nothing
detective (LiteralAnd (JustInput v2) (CarrryBit v1)) = if read (tail v1) + 1 == read (tail v2) then Just (CryInterm ('z':tail v2)) else Nothing
detective (LiteralAnd (ProbablyWrong _) _) = Nothing
detective (LiteralOr (ProbablyWrong _) _) = Nothing
detective (LiteralXor (ProbablyWrong _) _) = Nothing
detective (LiteralAnd _ (ProbablyWrong _)) = Nothing
detective (LiteralOr _ (ProbablyWrong _)) = Nothing
detective (LiteralXor _ (ProbablyWrong _)) = Nothing
detective pw = Just (ProbablyWrong pw)

toDetective gates ("OR", a, b) = do
    aRes <- HM.lookup a gates
    bRes <- HM.lookup b gates
    return (LiteralOr aRes bRes)
toDetective gates ("AND", a, b) = do
    aRes <- HM.lookup a gates
    bRes <- HM.lookup b gates
    return (LiteralAnd aRes bRes)
toDetective gates ("XOR", a, b) = do
    aRes <- HM.lookup a gates
    bRes <- HM.lookup b gates
    return (LiteralXor aRes bRes)

detectiveTilFixpoint gates current = do
        let toDetect = HM.mapMaybe (toDetective current) gates
        let detected = HM.mapMaybe detective toDetect
        let new = HM.union detected current
        if new == current
            then current
            else detectiveTilFixpoint gates new


isPw (ProbablyWrong _) = True
isPw _ = False

reverseLookup value = head . HM.keys . HM.filter (== value)

isWeird k (OutputBit v) = k /= v
isWeird k _ = False

nextz ('z':cur) = printf "z%02d" (read cur + 1 :: Int)
prevz ('z':cur) = printf "z%02d" (read cur - 1 :: Int)

getTarget (ProbablyWrong (LiteralXor a (CarrryBit v))) = Just (a, XorInterm (nextz v))
getTarget (ProbablyWrong (LiteralXor (CarrryBit v) a)) = Just (a, XorInterm (nextz v))
getTarget (ProbablyWrong (LiteralXor a (XorInterm v))) = Just (a, CarrryBit (prevz v))
getTarget (ProbablyWrong (LiteralXor (XorInterm v) a)) = Just (a, CarrryBit (prevz v))
-- getTarget (ProbablyWrong (LiteralAnd a (CryInterm v))) = Just (a, CryInterm v)
-- getTarget (ProbablyWrong (LiteralAnd (CryInterm v) a)) = Just (a, CryInterm v)
getTarget (ProbablyWrong _) = Nothing -- error "I can't solve this case!"

swap wire1 wire2 gates = do
    let gate1 = unwrap $ HM.lookup wire1 gates
    let gate2 = unwrap $ HM.lookup wire2 gates
    HM.insert wire2 gate1 (HM.insert wire1 gate2 gates)

tryFix :: HM.HashMap String DetectiveResult -> HM.HashMap String (String, String, String) -> IO [(String, String)]
tryFix detectiveStart detectiveGates = do
    let detectiveResult = detectiveTilFixpoint detectiveGates detectiveStart
    let easySwaps = map (\(a, OutputBit b) -> (a, b)) . HM.toList $ HM.filterWithKey isWeird detectiveResult
    let easySwapped = foldr (uncurry swap) detectiveGates easySwaps
    print easySwaps
    if HM.size detectiveResult == HM.size detectiveStart + HM.size detectiveGates
        then return []
    else if easySwaps /= []
        then fmap (easySwaps ++) (tryFix detectiveStart easySwapped)
    else do
        let hints = HM.filter isPw detectiveResult
        let swaps = map (bimap (`reverseLookup` detectiveResult) (`reverseLookup` detectiveResult)) . HM.elems $ HM.mapMaybe getTarget hints
        print swaps
        let swapped = foldr (uncurry swap) detectiveGates swaps
        fmap (swaps ++) (tryFix detectiveStart swapped)


main = do
    handle <- openFile "day24.txt" ReadMode
    [inputs, gates] <- fmap (map lines . splitOn "\n\n") (hGetContents handle)
    let inputMap = HM.fromList $ map (toPair . splitOn ": ") inputs
    -- let gateList = map (toGate . words) gates
    -- let gateMap = HM.fromList $ map toPaired gateList
    -- let wireSet = HS.fromList $ concatMap (toSet . words) gates
    -- let result = applyTilFixed wireSet gateList inputMap
    -- let zs = HM.filterWithKey (\k v -> head k == 'z') result
    -- let hasSusInputs = HM.keys (HM.filter (not . isOkay) result)
    -- let susInputs = nub . concatMap (unwrap . (`HM.lookup` gateMap)) $ hasSusInputs
    -- print hasSusInputs
    -- print (HM.fromList $ map (\k -> (k, unwrap $ HM.lookup k gateMap)) hasSusInputs)
    -- print susInputs
    let detectiveStart = HM.fromList $ map (\k -> (k, JustInput k)) (HM.keys inputMap)
    let detectiveGates = HM.fromList $ map (toGate2 . words) gates
    fixes <- tryFix detectiveStart detectiveGates
    putStrLn (intercalate "," . sort . concatMap (\(a, b) -> [a, b]) $ fixes)
    -- let detectiveResult = detectiveTilFixpoint detectiveGates detectiveStart
    -- print (HM.filterWithKey isWeird detectiveResult)
    -- print detectiveResult
    -- print (HM.filter isPw detectiveResult)
    -- print (HS.difference (HS.fromList hasSusInputs) (HM.keysSet detectiveResult))
    hClose handle