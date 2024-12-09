import System.IO
import Control.Monad
import Data.List
import Data.List.Index
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Internal.Strict (mapMaybe)
import Debug.Trace
import Data.Maybe

toNum '0' = 0
toNum '1' = 1
toNum '2' = 2
toNum '3' = 3
toNum '4' = 4
toNum '5' = 5
toNum '6' = 6
toNum '7' = 7
toNum '8' = 8
toNum '9' = 9

empties [] = 0
empties [a] = 0
empties (a:b:xs) = toNum b + empties xs

fulls _ [] = []
fulls idx [a] = toNum a `replicate` Just idx
fulls idx (a:b:xs) = (toNum a `replicate` Just idx) ++ (toNum b `replicate` Nothing) ++ fulls (idx + 1) xs

zipper [] _ = []
zipper (Just a:xs) l = Just a : zipper xs l
zipper (Nothing:xs) [] = Nothing : xs
zipper (Nothing:xs) (a:zxs) = Just a : zipper xs zxs

main = do
        handle <- openFile "day9.txt" ReadMode
        compacted <- hGetContents handle
        -- let compacted = "2333133121414131402"
        let decompacted = fulls 0 compacted
        let fulls = catMaybes decompacted
        let finLen = length fulls
        let filled = (catMaybes . take finLen) (zipper decompacted (reverse fulls))
        let checksum = sum (zipWith (*) [0..finLen - 1] filled)
        print checksum
        hClose handle