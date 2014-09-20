import VMU
import VMUFile
import System.Environment
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Text.Printf

-- List all files in the file system

listFiles :: VMU -> String
listFiles vmu = 
    (printf titleFormat "" "Name" "Type" "Size" "StartBlock" "CopyProtected") ++
    (listFiles' (catMaybes $ files vmu) 1 fileFormat)

    where titleFormat = "%5s  %-10s  %-4s  %-4s  %-10s  %-13s\n" 
          fileFormat  = "%2d:  %-11s  %-4s  %-4s  %-10s  %-13s\n"

listFiles' :: [DirectoryEntry] -> Int -> String -> String
listFiles' [] _ _ = ""
listFiles' (x:xs) no format =
    (printf format no fName fType fSize fStart fCopy)  ++ 
        (listFiles' xs (no + 1) format)
    where 
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

-- Inject a nexus DCI format save file into the filesystem

injectDCI :: BS.ByteString -> BS.ByteString -> Either String VMU
injectDCI vmuBs file = do
    vmu <- createVMU vmuBs 
    injectDCIFile (BS.unpack file) vmu 


injectDCICommand :: [String] -> IO()
injectDCICommand args
    | length args < 3 = putStrLn "Expecting vmu and dci file"
    | otherwise = do
        vmuBs <- BS.readFile $ args !! 1
        fileBs <- BS.readFile $ args !! 2
        case injectDCI vmuBs fileBs of
            Left  x -> putStrLn x
            Right v ->  BS.writeFile (args !! 1) $ BS.pack $ exportVMU v


-- Extract a file from the filesystem in the nexus DCI format

extractDCI :: Int -> BS.ByteString -> Either String VMUFile
extractDCI fileNo vmuBs = do
    vmu <- createVMU vmuBs
    fInfo <- getEntry fileNo vmu
    fileRaw <- rawDumpFile fileNo vmu
    return $ createVMUFileDCI fInfo fileRaw    


extractDCICommand :: [String] -> IO()
extractDCICommand args
    | length args < 4 = putStrLn "Expecting vmu file, dci file number and output file name"
    | otherwise = do        
        vmuBs <- BS.readFile $ args !! 1
        let fileNo = read (args !! 2) :: Int
        case extractDCI fileNo vmuBs of
            Left x -> putStrLn x
            Right v -> BS.writeFile (args !! 3) $ BS.pack $ exportVMUFile $ v


-- Unlock unused blocks 200 - 240 in the filesystem for use

unlockBlocks :: BS.ByteString -> Either String VMU
unlockBlocks vmuBs = do
    vmu <- createVMU vmuBs
    let rootBlock = root vmu
    let newRoot = rootBlock {userBlocksCount = 241}
    return vmu {root = newRoot}  

unlockBlocksCommand :: [String] -> IO()
unlockBlocksCommand args 
    | length args < 2 = putStrLn "Expecting vmu file"
    | otherwise = do
        vmuBs <- BS.readFile $ args !! 1
        case unlockBlocks vmuBs of
            Left x -> putStrLn x
            Right v ->  BS.writeFile (args !! 1) $ BS.pack $ exportVMU v

executeCommand :: String -> [String] -> IO() 
executeCommand command args = case args !! 0 of
    "ls" -> listFilesCommand args
    "injectDCI" -> injectDCICommand args 
    "extractDCI" -> extractDCICommand args
    "unlockBlocks" -> unlockBlocksCommand args 
    _ -> error $ "unknown command " ++ command

main :: IO()
main = do 
        args <- getArgs
        progName <- getProgName
        
        let command = head args
             
        if length args <= 0 
            then putStrLn ("Usage " ++ progName ++ " [vmu file]")
            else executeCommand command args
