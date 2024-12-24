import System.IO
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Split (splitOn)
import Data.Function

toTup [a, b] = [(a, HS.singleton b), (b, HS.singleton a)]

bronKerbosch links r p x
    | HS.null p && HS.null x = [r]
    | otherwise = do
        let n v = HM.lookupDefault HS.empty v links
        let inner v (acc, p, x) = do
                let new = bronKerbosch links (HS.insert v r) (p `HS.intersection` n v) (x `HS.intersection` n v)
                (new ++ acc, HS.delete v p, HS.insert v x)
        let minima = minimumBy (compare `on` length) $ HS.map ((p `HS.difference`) . n) (p `HS.union` x)
        let (res, _, _) = HS.foldr inner ([], p, x) minima
        res

main = do
    handle <- openFile "day23.txt" ReadMode
    codes <- fmap (map (sort . splitOn "-") . lines) (hGetContents handle)
    let boxes = HS.unions (map HS.fromList codes)
    let linkMap = HM.fromListWith HS.union (concatMap toTup codes)
    let maxSet = maximumBy (compare `on` HS.size) $ bronKerbosch linkMap HS.empty boxes HS.empty
    putStrLn (intercalate "," . sort . HS.toList $ maxSet)
    hClose handle