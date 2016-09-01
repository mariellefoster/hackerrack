import Control.Applicative
import Control.Monad
import System.IO

sumDiag :: [[Int]] -> Int -> Int -> Int
sumDiag rows s 0 = s 
sumDiag rows s n = sumDiag rows (s + ((!!) ((!!) rows (n-1)) (n-1))) (n - 1)


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getMultipleLines n
    let a = map ( map ( read :: String -> Int ) . words ) a_temp
    let f = (\ x -> reverse x)
    let l2r = sumDiag a 0 (length a)
    let r2l = sumDiag (map f a) 0 (length a)
    putStrLn $ show $ (abs $ l2r - r2l)
    
    

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          


