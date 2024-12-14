import System.IO
import Data.List
import Text.Regex.TDFA
import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Control.Monad
pat = "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)"

stepBot w h [x, y, dx, dy] = [(x + dx) `mod` w, (y + dy) `mod` h, dx, dy]

stepBotN w h 1 l = stepBot w h l
stepBotN w h n l = stepBotN w h (n - 1) (stepBot w h l)

inc Nothing = Just 1
inc (Just n) = Just (n + 1)

mappify [] = HM.empty
mappify ([x, y, _, _]:xs) = HM.alter inc (x, y) (mappify xs)


vizmap w h mappified n = show n ++ concatMap (vizrow w mappified) [0..h - 1]
    where
        vizrow w mappified y = '\n':map (vizpoint mappified y) [0..w - 1]
        vizpoint mappified y x = vizchar (HM.lookup (x, y) mappified)
        vizchar Nothing = ' '
        vizchar (Just n) = '#'

recurStep n width height machines = do
    let viz = vizmap width height . mappify
    let vizd = viz machines n
    when ("##################" `isInfixOf` vizd) $ do
        putStrLn vizd
        threadDelay 50000
    recurStep (n + 1) width height (map (stepBot width height) machines)

main = do
        handle <- openFile "day14.txt" ReadMode
        let width = 101;
        let height = 103;
        machines <- fmap (map (map read . tail . (\e -> getAllTextSubmatches (e =~ pat))) . lines) (hGetContents handle) :: IO [[Int]]
        recurStep 0 width height machines
        hClose handle