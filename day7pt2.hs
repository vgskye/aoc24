import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Control.Parallel.Strategies (rpar, parMap)
import Math.NumberTheory.Logarithms

parse :: [String] -> (Integer, [Integer])
parse [target, numbers] = (read target, (map read . words) numbers)

concatNum :: Integer -> Integer -> Integer
concatNum a b = a * (10 ^ ((1 +) . fromIntegral . integerLog10') b) + b

genAll target [x] = [x]
genAll target (cur:rest) =
        let prev = filter (target >=) (genAll target rest)
        in map (cur *) prev ++ map (cur +) prev ++ map (`concatNum` cur) prev

isOkay (target, numbers) = target `elem` (genAll target . reverse) numbers

mapOkay (target, numbers) = if isOkay (target, numbers) then target else 0

main = do
        handle <- openFile "day7.txt" ReadMode
        rules <- fmap (map (parse . splitOn ": ") . lines) (hGetContents handle)
        print ((sum . parMap rpar mapOkay) rules)
        hClose handle