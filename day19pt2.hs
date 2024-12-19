import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import GHC.Bits
import Text.Regex.TDFA
import Data.List.Split
import Data.Maybe
import Data.MemoTrie

procStringI _ "" = 1
procStringI l s = sum . map (procString l) . mapMaybe (`stripPrefix` s) $ l

procString = memo2 procStringI

main = do
    handle <- openFile "day19.txt" ReadMode
    [towelstr, patternstr] <- fmap (splitOn "\n\n") (hGetContents handle)
    let patterns = lines patternstr
    let towels = splitOn ", " towelstr
    print $ (sum . map (procString towels)) patterns
    hClose handle
