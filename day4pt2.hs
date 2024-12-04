import System.IO
import Control.Monad
import Data.List

cons (x:xs) = xs

matches (
        ('M': _ :'M':_):
        ( _ :'A': _ :_):
        ('S': _ :'S':_):_
        ) = 1
matches (
        ('M': _ :'S':_):
        ( _ :'A': _ :_):
        ('M': _ :'S':_):_
        ) = 1
matches (
        ('S': _ :'M':_):
        ( _ :'A': _ :_):
        ('S': _ :'M':_):_
        ) = 1
matches (
        ('S': _ :'S':_):
        ( _ :'A': _ :_):
        ('M': _ :'M':_):_
        ) = 1
matches _ = 0

searchRow ([]:xs) = 0
searchRow l = matches l + searchRow (map cons l)

findMatch [] = 0
findMatch l = searchRow l + findMatch (cons l)

main = do
        handle <- openFile "day4.txt" ReadMode
        search <- fmap lines (hGetContents handle)
        print (findMatch search)
        hClose handle