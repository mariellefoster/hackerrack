import Control.Applicative
import Control.Monad
import System.IO
import qualified Data.Text as T



main :: IO ()
main = do
    time <- getLine    
    let timeLst = show $ T.splitOn (T.pack ":") (T.pack time)
    putStrLn timeLst
    

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

