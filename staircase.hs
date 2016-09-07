mport Control.Applicative
import Control.Monad
import System.IO
import Data.Maybe

recurseStaircase :: Int -> Int -> IO ()
recurseStaircase 0 m = return ()
recurseStaircase n m = do
    putStr $ replicate (n-1) ' '
    putStrLn $ replicate m '#'
    recurseStaircase (n-1) (m+1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    recurseStaircase n 1

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          


