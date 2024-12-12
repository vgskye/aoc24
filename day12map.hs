import System.IO
import Control.Monad
import Data.List
import qualified Data.HashSet as HS
import Debug.Trace
import Data.Maybe
main = do
        handle <- openFile "day12.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let regions = (nub . sort . concat) origmap
        putStrLn "P2"
        print width
        print height
        print (length regions - 1)
        let pgmd = concatMap (mapMaybe (`elemIndex` regions)) origmap
        let stringified = foldr (\v acc -> show v ++ ' ':acc) "" pgmd
        putStrLn stringified
        hClose handle