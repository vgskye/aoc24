import System.IO
import Control.Monad
import Data.List
import Text.Regex.TDFA
import Text.Regex.TDFA.Common (Regex(Regex))

xmaspat = makeRegex "XMAS" :: Regex

main = do
        handle <- openFile "day4.txt" ReadMode
        search <- fmap lines (hGetContents handle)
        let trans = transpose search
        let diagPad = map (`replicate` '-') [0..length search - 1]
        let padded = zipWith (++) diagPad search
        let paddedrev = zipWith (++) (reverse diagPad) search
        let searchFn = sum . map (matchCount xmaspat)
        let searchRev = sum . map (matchCount xmaspat . reverse)
        let searchAll a = searchFn a + searchRev a
        print ((sum . map searchAll) [search, trans, transpose padded, transpose paddedrev])
        hClose handle