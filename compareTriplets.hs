import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    a0_temp <- getLine
    let a0_t = words a0_temp
    let a0 = read $ a0_t!!0 :: Int
    let a1 = read $ a0_t!!1 :: Int
    let a2 = read $ a0_t!!2 :: Int
    b0_temp <- getLine
    let b0_t = words b0_temp
    let b0 = read $ b0_t!!0 :: Int
    let b1 = read $ b0_t!!1 :: Int
    let b2 = read $ b0_t!!2 :: Int
    let f = (\ x y -> if x > y then 1 else 0)
    let alice_res = sum $ zipWith f [a0, a1, a2] [b0, b1, b2]
    let bob_res = sum $ zipWith f [b0, b1, b2] [a0, a1, a2]
    putStr $ show alice_res
    putStr " "
    putStr $ show bob_res
    
    
    

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret    