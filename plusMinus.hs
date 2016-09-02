import Control.Applicative
import Control.Monad
import System.IO

numType :: [Int]  -> (Int, Int, Int) -> (Int, Int, Int)
numType [] (a, b, c) = (a, b, c)
numType arr (a, b, c)
    | ((head arr) == 0) = numType (tail arr)  (a, b, c + 1)
    | ((head arr) > 0) = numType (tail arr) (a + 1, b, c)
    | ((head arr) < 0) = numType (tail arr)  (a, b + 1, c)
    


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Int]
    let (a, b, c) = numType arr (0,0,0)
    let len = length arr
    putStrLn $ show $ (fromIntegral a / fromIntegral len)
    putStrLn $ show $ (fromIntegral b / fromIntegral len)
    putStrLn $ show $ (fromIntegral c / fromIntegral len)

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

