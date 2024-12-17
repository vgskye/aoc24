import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import GHC.Bits
import Text.Regex.TDFA
import Data.List.Split

data ProcessorState = ProcessorState (Int, Int, Int) Int deriving (Eq, Show)
pat = "Register A: ([0-9]+)\nRegister B: ([0-9]+)\nRegister C: ([0-9]+)\n\nProgram: ([0-7,]+)"

getCompound 0 _ = 0
getCompound 1 _ = 1
getCompound 2 _ = 2
getCompound 3 _ = 3
getCompound 4 (ProcessorState (a, b, c) pc) = a
getCompound 5 (ProcessorState (a, b, c) pc) = b
getCompound 6 (ProcessorState (a, b, c) pc) = c
getCompound 7 _ = error "whuh?"

runinsn 0 op (ProcessorState (a, b, c) pc) =
    return $ ProcessorState (a `shiftR` getCompound op (ProcessorState (a, b, c) pc), b, c) (pc + 2)
runinsn 1 op (ProcessorState (a, b, c) pc) =
    return $ ProcessorState (a, b `xor` op, c) (pc + 2)
runinsn 2 op (ProcessorState (a, b, c) pc) =
    return $ ProcessorState (a, getCompound op (ProcessorState (a, b, c) pc) `mod` 8, c) (pc + 2)
runinsn 3 op (ProcessorState (0, b, c) pc) =
    return $ ProcessorState (0, b, c) (pc + 2)
runinsn 3 op (ProcessorState (a, b, c) pc) =
    return $ ProcessorState (a, b, c) op
runinsn 4 op (ProcessorState (a, b, c) pc) =
    return $ ProcessorState (a, b `xor` c, c) (pc + 2)
runinsn 5 op (ProcessorState (a, b, c) pc) = do
    print $ getCompound op (ProcessorState (a, b, c) pc) `mod` 8
    return $ ProcessorState (a, b, c) (pc + 2)
runinsn 6 op (ProcessorState (a, b, c) pc) =
    return $ ProcessorState (a, a `shiftR` getCompound op (ProcessorState (a, b, c) pc), c) (pc + 2)
runinsn 7 op (ProcessorState (a, b, c) pc) =
    return $ ProcessorState (a, b, a `shiftR` getCompound op (ProcessorState (a, b, c) pc)) (pc + 2)

runprog l state = do
    let ProcessorState _ pc = state
    if pc < 0 || pc >= length l
        then return ()
        else runinsn (l !! pc) (l !! (pc + 1)) state >>= runprog l

main = do
    handle <- openFile "day17.txt" ReadMode
    [_, a, b, c, insns] <- fmap (\e -> getAllTextSubmatches (e =~ pat)) (hGetContents handle)
    let [newA, newB, newC] = map read [a, b, c]
    let newInsns = (map read . splitOn ",") insns
    runprog newInsns (ProcessorState (newA, newB, newC) 0)
    hClose handle
