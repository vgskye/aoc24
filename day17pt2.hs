import System.IO
import GHC.Bits
import Text.Regex.TDFA
import Data.List.Split

data ProcessorState = ProcessorState (Int, Int, Int) Int [Int] deriving (Eq, Show)
pat = "Register A: ([0-9]+)\nRegister B: ([0-9]+)\nRegister C: ([0-9]+)\n\nProgram: ([0-7,]+)"

getCompound 0 _ = 0
getCompound 1 _ = 1
getCompound 2 _ = 2
getCompound 3 _ = 3
getCompound 4 (ProcessorState (a, b, c) pc disp) = a
getCompound 5 (ProcessorState (a, b, c) pc disp) = b
getCompound 6 (ProcessorState (a, b, c) pc disp) = c
getCompound 7 _ = error "whuh?"

runinsn 0 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a `shiftR` getCompound op (ProcessorState (a, b, c) pc disp), b, c) (pc + 2) disp
runinsn 1 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a, b `xor` op, c) (pc + 2) disp
runinsn 2 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a, getCompound op (ProcessorState (a, b, c) pc disp) `mod` 8, c) (pc + 2) disp
runinsn 3 op (ProcessorState (0, b, c) pc disp) =
    ProcessorState (0, b, c) (pc + 2) disp
runinsn 3 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a, b, c) op disp
runinsn 4 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a, b `xor` c, c) (pc + 2) disp
runinsn 5 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a, b, c) (pc + 2) ((getCompound op (ProcessorState (a, b, c) pc disp) `mod` 8):disp)
runinsn 6 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a, a `shiftR` getCompound op (ProcessorState (a, b, c) pc disp), c) (pc + 2) disp
runinsn 7 op (ProcessorState (a, b, c) pc disp) =
    ProcessorState (a, b, a `shiftR` getCompound op (ProcessorState (a, b, c) pc disp)) (pc + 2) disp

runprog l state = do
    let ProcessorState _ pc disp = state
    if pc < 0 || pc >= length l
        then disp
        else runprog l (runinsn (l !! pc) (l !! (pc + 1)) state)

prefixSize [] [] = 0
prefixSize [] _ = 0
prefixSize _ [] = 0
prefixSize (a:axs) (b:bxs)
    | a == b = 1 + prefixSize axs bxs
    | otherwise = 0


guess sim max l valid
    | l > max = head valid
    | otherwise =
        guess sim max (l + 1) (concatMap (filter (\e -> sim e == l) . (\e -> map ((8 * e) +) [0..7])) valid)

main = do
    handle <- openFile "day17.txt" ReadMode
    [_, a, b, c, insns] <- fmap (\e -> getAllTextSubmatches (e =~ pat)) (hGetContents handle)
    let [newA, newB, newC] = map read [a, b, c]
    let newInsns = (map read . splitOn ",") insns
    let isOverQuine regA = length newInsns < length (runprog newInsns (ProcessorState (1 `shiftL` regA, newB, newC) 0 []))
    let scoring regA = reverse newInsns `prefixSize` runprog newInsns (ProcessorState (regA, newB, newC) 0 [])
    let maxBits = (head . filter isOverQuine) [0..]
    let octets = maxBits `div` 3
    print (guess scoring octets 0 [0])
    hClose handle
