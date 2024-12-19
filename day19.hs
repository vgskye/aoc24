import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import GHC.Bits
import Text.Regex.TDFA
import Data.List.Split

prep [] = []
prep (',':' ':xs) = '|':prep xs
prep (a:xs) = a:prep xs

main = do
    handle <- openFile "day19.txt" ReadMode
    [towelstr, patternstr] <- fmap (splitOn "\n\n") (hGetContents handle)
    let patterns = lines patternstr
    let pat = "^(" ++ prep towelstr ++ ")+$"
    print $ (length . filter (=~ pat)) patterns
    hClose handle
