{-# LANGUAGE DeriveDataTypeable, TypeFamilies, OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Main where

import qualified Data.ByteString.Lazy as BS
import           Data.Maybe
import           System.Environment
import           Text.Printf
import           Data.Typeable as T
import           Data.Bits
import           Data.Word
import           Control.Concurrent
import           Data.Proxy
import           Graphics.QML
import           VMU
import           VMUFile
import           Operations

{-
listFiles :: VMU -> String
listFiles vmu =
    printf titleFormat "" "Name" "Type" "Size" "StartBlock" "CopyProtected" ++
    listFiles' (catMaybes $ files vmu) 1 fileFormat

    where titleFormat = "%5s  %-10s  %-4s  %-4s  %-10s  %-13s\n"
          fileFormat  = "%2d:  %-11s  %-4s  %-4s  %-10s  %-13s\n"

listFiles' :: [DirectoryEntry] -> Int -> String -> String
listFiles' [] _ _ = ""
listFiles' (x:xs) no format =
    printf format no fName fType fSize fStart fCopy  ++
        listFiles' xs (no + 1) format
    where
          fName =  fileName x
          fType =  (show . fileType) x
          fSize =  (show . sizeInBlocks) x
          fStart = (show . startingBlock) x
          fCopy =  if copyProtected x then "Yes" else "No"


-- | List all files in the file system
listFilesCommand :: [String] -> IO ()
listFilesCommand args
    | length args < 2 = putStrLn "Expecting vmu file"
    | otherwise = do
        bs <- BS.readFile $ args !! 1
        case createVMU bs of
            Left str -> error str
            Right vmu -> putStrLn $ listFiles vmu


-- | Remove a file from the file system
rmCommand :: [String] -> IO()
rmCommand args
    | length args < 3 = putStrLn "Expecting vmu file and file no"
    | otherwise = do
        bs <- BS.readFile $ args !! 1
        let fileNo = read $ args !! 2
        case rm fileNo bs of
            Left x -> putStrLn x
            Right v -> BS.writeFile (args !! 1) $ BS.pack $ exportVMU v


-- | Inject a Nexus DCI save into the file system
injectDCICommand :: [String] -> IO()
injectDCICommand args
    | length args < 3 = putStrLn "Expecting vmu and dci file"
    | otherwise = do
        vmuBs <- BS.readFile $ args !! 1
        fileBs <- BS.readFile $ args !! 2
        case injectDCI vmuBs fileBs of
            Left  x -> putStrLn x
            Right v ->  BS.writeFile (args !! 1) $ BS.pack $ exportVMU v


-- | Extract a file in the file system in Nexus DCI save format
extractDCICommand :: [String] -> IO()
extractDCICommand args
    | length args < 4 = putStrLn "Expecting vmu file, dci file number and output file name"
    | otherwise = do
        vmuBs <- BS.readFile $ args !! 1
        let fileNo = read (args !! 2) :: Int
        case extractDCI fileNo vmuBs of
            Left x -> putStrLn x
            Right v -> BS.writeFile (args !! 3) $ BS.pack $ exportVMUFile v


-- | Unlock the extra 41 blocks of memory on the VMU
unlockBlocksCommand :: [String] -> IO()
unlockBlocksCommand args
    | length args < 2 = putStrLn "Expecting vmu file"
    | otherwise = do
        vmuBs <- BS.readFile $ args !! 1
        case unlockBlocks vmuBs of
            Left x -> putStrLn x
            Right v ->  BS.writeFile (args !! 1) $ BS.pack $ exportVMU v


-- | Display options
helpCommand :: IO ()
helpCommand = do
   putStrLn "Commands:"
   putStrLn "ls VMUFILE  --List all files in the VMU"
   putStrLn "rm VMUFILE FILENO  --Remove the specified file from the VMU"
   putStrLn "injectDCI VMUFILE DCIFILE   --Inject a DCI save file into the VMU"
   putStrLn $ "extractDCI VMUFILE FILENO OUTFILE  --Extract the specified file " ++
            "from the VMU and save it in DCI format as the specifed out file"
   putStrLn $ "unlockBlocks VMUFILE  --Unlock the unused 41 blocks on the VMU for" ++
        " extra storage space"


executeCommand :: String -> [String] -> IO()
executeCommand command args = case head args of
    "ls" -> listFilesCommand args
    "rm" -> rmCommand args
    "injectDCI" -> injectDCICommand args
    "extractDCI" -> extractDCICommand args
    "unlockBlocks" -> unlockBlocksCommand args
    "help" -> helpCommand
    _ -> error $ "unknown command " ++ command ++
        "\nEnter \"help\" for a list of commands"

-}

newtype VMULoaded = VMULoaded (Maybe VMU)

noVMU :: VMULoaded
noVMU = VMULoaded Nothing

deriving instance Typeable VMULoaded

data ContextVMU = ContextVMU
                { _vmu :: MVar (ObjRef VMULoaded)
                } deriving Typeable


-- Signals
data VMUChanged deriving Typeable

instance SignalKeyClass VMUChanged where
    type SignalParams VMUChanged = IO ()


instance DefaultClass ContextVMU where
    classMembers = [
        defPropertySigRO "vmu" (Proxy :: Proxy VMUChanged) $ readMVar . _vmu . fromObjRef
       ,defMethod "addFile" addFile]


addFile :: ObjRef ContextVMU -> IO () 
addFile co = print "Add File Signalled"

instance DefaultClass VMULoaded where
    classMembers = []


main :: IO()
main = do

    l <- newMVar =<< newObjectDC noVMU -- Start off with no VMU
    tc <- newObjectDC $ ContextVMU l

    runEngineLoop defaultEngineConfig {
      initialDocument = fileDocument "qml/potato_tool.qml"
    , contextObject = Just $ anyObjRef tc
    }
