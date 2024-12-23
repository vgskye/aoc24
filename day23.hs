import System.IO
import qualified Data.HashSet as HS
import Data.List
import Data.List.Split (splitOn)
import Combinatorics

toTup [a, b] = (a, b)

tries [a, b, c] = HS.fromList $ map sort [[a, b], [b, c], [c, a]]

main = do
    handle <- openFile "day23.txt" ReadMode
    codes <- fmap (map (sort . splitOn "-") . lines) (hGetContents handle)
    let boxes = HS.unions (map HS.fromList codes)
    let codeSet = HS.fromList codes
    let triples = map tries (filter (elem 't' . map head) (tuples 3 (HS.toList boxes)))
    print (length . filter (`HS.isSubsetOf` codeSet) $ triples)
    hClose handle