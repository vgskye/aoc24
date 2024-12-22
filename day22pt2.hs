import System.IO
import Data.Bits
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import Data.Function
import Control.Parallel.Strategies (parMap, rpar)

step :: Int -> Int
step n = do
    let i1 = ((n `shiftL` 6) `xor` n) `mod` 16777216
    let i2 = ((i1 `shiftR` 5) `xor` i1) `mod` 16777216
    let i3 = ((i2 `shiftL` 11) `xor` i2) `mod` 16777216
    i3

stepN :: Int -> Int -> [Int]
stepN 0 a = [a]
stepN n a = step (head $ stepN (n - 1) a):stepN (n - 1) a

pairs fn [] = []
pairs fn [_] = []
pairs fn (a:b:xs) = fn a b:pairs fn (b:xs)

quads [_, _, _] = []
quads (a:b:c:d:xs) = (a, b, c, d):quads (b:c:d:xs)

skip 0 l = l
skip n (a:xs) = skip (n - 1) xs

diffs n = do
    let prices = map (`mod` 10) (stepN 2000 n)
    let diffs = pairs (-) prices
    HM.fromList $ nubBy ((==) `on` fst) (zip (quads (reverse diffs)) (skip 4 . reverse $ prices))

main = do
    handle <- openFile "day22.txt" ReadMode
    codes <- fmap (map read . lines) (hGetContents handle) :: IO [Int]
    let deals = parMap rpar diffs codes
    let negs = HS.unions (parMap rpar HM.keysSet deals)
    let rateNeg neg = sum . parMap rpar (HM.lookupDefault 0 neg) $ deals
    let maxNeg = maximumBy (compare `on` rateNeg) negs
    print (rateNeg maxNeg)
    hClose handle