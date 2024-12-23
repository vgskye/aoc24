import System.IO
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Split (splitOn)
import Data.Function

toTup [a, b] = [(a, HS.singleton b), (b, HS.singleton a)]

bronKerbosch links r p x
    | HS.null p && HS.null x = HS.singleton r
    | HS.null p = HS.empty
    | otherwise = do
        let v = head $ HS.toList p
        let nv = HM.lookupDefault HS.empty v links
        let cur = bronKerbosch links (HS.insert v r) (p `HS.intersection` nv) (x `HS.intersection` nv)
        let recur = bronKerbosch links r (HS.delete v p) (HS.insert v x)
        HS.union cur recur

main = do
    handle <- openFile "day23.txt" ReadMode
    codes <- fmap (map (sort . splitOn "-") . lines) (hGetContents handle)
    let boxes = HS.unions (map HS.fromList codes)
    let linkMap = HM.fromListWith HS.union (concatMap toTup codes)
    let maxSet = maximumBy (compare `on` HS.size) $ bronKerbosch linkMap HS.empty boxes HS.empty
    putStrLn (intercalate "," . sort . HS.toList $ maxSet)
    hClose handle