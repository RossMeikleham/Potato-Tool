import Data.Word
import Data.Binary
import System.IO
import System.Environment
import GHC.IO.Handle.FD

{-data Timestamp = 
    { century :: Word8
    , year :: Word8
    , month :: Word8
    , day :: Word8
    , hour :: Word8
    , minute :: Word8
    , second :: Word8
    , dayOfWeek :: Word8
    }
-}

{-data RootBlock =
    customVMSColor 1/0
    blueVMS 8
    redVMS 8
    greenVMS 8
    alphaComponent 8
    timeStamp
    locationFAT 16
    sizeFAT 16
    locationDirectory 16
    sizeDirectory
    iconShape 0-123
    userBlocks 200
-}
data Directory = Directory { entries :: [DirectoryEntry]} deriving Show


data DirectoryEntry = DirectoryEntry 
    { fileType :: FileType
    , copyProtected :: Bool
    , startingBlock :: Word16
    , fileName :: String
 --   , timestamp :: Timestamp
    , sizeInBlocks :: Word16
    , offsetInBlocks :: Word16
    } deriving Show

data FileType = NoFile | Game | Data deriving Show

vmuSize = 128 * 1024 --128KB vmu


main :: IO()
main = do 
        args <- getArgs
        progName <- getProgName

        let vmu = head args
             
        if length args <= 0 
            then putStrLn ("Usage " ++ progName ++ " [vmu file]")
            else do 
                 file <- openFile vmu ReadMode
                 size <- hFileSize file
                 if size /= vmuSize
                     then putStrLn ("VMU is incorrect size (" ++ (show size) ++ " bytes)")
                     else putStrLn "ok"
                 
                 
                  
