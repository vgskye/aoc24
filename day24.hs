import System.IO
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Split (splitOn)
import Data.Bits

toPair [k, "0"] = (k, False)
toPair [k, "1"] = (k, True)

a ⊻ b = (a || b) && not (a && b)

toGate [a, "OR", b, "->", out] = (a, (||), b, out)
toGate [a, "AND", b, "->", out] = (a, (&&), b, out)
toGate [a, "XOR", b, "->", out] = (a, (⊻), b, out)

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

main = do
    handle <- openFile "day24.txt" ReadMode
    [inputs, gates] <- fmap (map lines . splitOn "\n\n") (hGetContents handle)
    let inputMap = HM.fromList $ map (toPair . splitOn ": ") inputs
    let gateList = map (toGate . words) gates
    let wireSet = HS.fromList $ concatMap (toSet . words) gates
    let result = applyTilFixed wireSet gateList inputMap
    let zs = HM.filterWithKey (\k v -> head k == 'z') result
    let znums = HM.mapKeys (read . tail) zs :: HM.HashMap Int Bool
    let number = sum . map (\(k, v) -> fromEnum v `shiftL` k) . HM.toList $ znums
    print number
    hClose handle