import VMU
import System.IO
import System.Environment
import GHC.IO.Handle.FD
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Text.Printf



listFiles :: VMU -> String
listFiles vmu = 
    (printf titleFormat "" "Name" "Type" "Size" "StartBlock" "CopyProtected") ++
    (listFiles' (catMaybes $ files vmu) 0 fileFormat)

    where titleFormat = "%5s  %-10s  %-4s  %-4s  %-10s  %-13s\n" 
          fileFormat  = "%2d:  %-11s  %-4s  %-4s  %-10s  %-13s\n"

listFiles' :: [DirectoryEntry] -> Int -> String -> String
listFiles' [] no format = ""
listFiles' (x:xs) no format =
    (printf format fNo fName fType fSize fStart fCopy)  ++ 
        (listFiles' xs (no+1) format)
    where 
          fNo = no
          fName =  fileName x
          fType =  (show . fileType) x
          fSize =  (show . sizeInBlocks) x
          fStart = (show . startingBlock) x
          fCopy =  if copyProtected x then "Yes" else "No"




listFilesCommand :: [String] -> IO ()
listFilesCommand args 
    | length args < 2 = putStrLn "Expecting vmu file"
    | otherwise = do
        bs <- BS.readFile $ args !! 1 
        case createVMU bs of
            Left str -> error str
            Right vmu -> putStrLn $ listFiles vmu

executeCommand :: String -> [String] -> IO() 
executeCommand command args = case args !! 0 of
    "ls" -> listFilesCommand args
    _ -> error $ "unknown command " ++ command

main :: IO()
main = do 
        args <- getArgs
        progName <- getProgName
        
        let command = head args
             
        if length args <= 0 
            then putStrLn ("Usage " ++ progName ++ " [vmu file]")
            else executeCommand command args
