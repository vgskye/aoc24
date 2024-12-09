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

data Fat = Occupied {id :: Int, size :: Int} | Free {size :: Int} deriving (Show)

fulls _ [] = []
fulls idx [a] = [Occupied idx (toNum a)]
fulls idx (a:b:xs) = Occupied idx (toNum a) : Free (toNum b) : fulls (idx + 1) xs

simplify [Free _] = []
simplify (Free a:Free b:xs) = simplify (Free (a + b) : xs)
simplify (Free 0:xs) = simplify xs
simplify (a:xs) = a : simplify xs
simplify [] = []

fits size (Occupied _ _) = False
fits size (Free freeBlock) = freeBlock >= size

findLastIndex p l = do
        idx <- findIndex p (reverse l)
        return (length l - (idx + 1))

isFree (Free _) = True
isFree _ = False

compactifiable size = any (fits size)

elemCompactifiable _ (Free _) = False
elemCompactifiable list (Occupied _ size) = compactifiable size list

swap list idx (Just freeIdx) id size rest = do
        let Free freeSize = list !! freeIdx
        let newFreeSize = freeSize - size
        let freeShortenedList = setAt freeIdx (Free newFreeSize) list
        let insertedList = insertAt freeIdx (Occupied id size) freeShortenedList
        let changedList = setAt (idx + 1) (Free size) insertedList
        if freeIdx < idx then simplify changedList else list
swap list idx Nothing id size rest = list

moveOccupied list idx rest = do
        let Occupied id size = list !! idx
        let freeIdx = findIndex (fits size) list
        doCompactify rest (swap list idx freeIdx id size rest)

hasId id (Occupied occid _) = id == occid
hasId _ (Free _) = False

doCompactify [] list = list
doCompactify (id:xs) list =
        let Just idx = findIndex (hasId id) list
        in moveOccupied list idx xs

getId (Occupied id _) = Just id
getId (Free _) = Nothing

compactify list = doCompactify (reverse (Data.Maybe.mapMaybe getId list)) list

checksumify _ [] = 0
checksumify idx (Free size:xs) = checksumify (idx + size) xs
checksumify idx (Occupied id 0:xs) = checksumify idx xs
checksumify idx (Occupied id size:xs) = (idx * id) + checksumify (idx + 1) (Occupied id (size - 1):xs)

showCompactified [] = []
showCompactified (Free size:xs) = (size `replicate` '.') ++ showCompactified xs
showCompactified (Occupied id size:xs) = (size `replicate` (last . show) id) ++ showCompactified xs

main = do
        handle <- openFile "day9.txt" ReadMode
        compacted <- hGetContents handle
        -- let compacted = "2333133121414131402"
        let compactified = compactify (fulls 0 compacted)
        let checksum = checksumify 0 compactified
        print checksum
        -- let checksum = sum (zipWith (*) [0..finLen - 1] filled)
        -- print checksum
        hClose handle