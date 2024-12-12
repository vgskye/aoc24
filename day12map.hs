import System.IO
import Data.List
import Data.Maybe
import Text.Printf (hPrintf)
main = do
        handle <- openFile "day12.txt" ReadMode
        writeHandle <- openFile "day12.pgm" WriteMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let regions = (nub . sort . concat) origmap
        hPrintf writeHandle "P2 %d %d %d " width height (length regions - 1)
        let pgmd = concatMap (mapMaybe (`elemIndex` regions)) origmap
        let stringified = foldr (\v acc -> show v ++ ' ':acc) "" pgmd
        hPutStrLn writeHandle stringified
        hClose writeHandle
        hClose handle