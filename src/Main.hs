{-# LANGUAGE DeriveDataTypeable, TypeFamilies, OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as TX
import           Data.Maybe
import           System.Environment
import           Text.Printf
import           Data.Typeable as T
import           Data.Bits
import           Data.Word
import           Control.Concurrent
import           Data.Proxy
import           Graphics.QML
import           Graphics.QML.Objects.ParamNames
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

data ContextVMU = ContextVMU
                { _vmu :: MVar (ObjRef VMU)
                } deriving Typeable

-- Signals

-- VMU updated/changed
data VMUChanged deriving Typeable

instance SignalKeyClass VMUChanged where
    type SignalParams VMUChanged = (IO ())
 
    
-- Error occured performing operations with the VMU
data VMUError deriving Typeable

instance SignalKeyClass VMUError where
    type SignalParams VMUError = (TX.Text -> IO())


-- Info message to deliver when performing operations with the VMU
data VMUInfoMsg deriving Typeable

instance SignalKeyClass VMUInfoMsg where
    type SignalParams VMUInfoMsg = (TX.Text -> IO())


instance DefaultClass ContextVMU where
    classMembers = [
        defPropertySigRO "self" (Proxy :: Proxy VMUChanged) ((\x -> return x) 
                :: ObjRef ContextVMU -> IO (ObjRef ContextVMU)),

        defPropertySigRO "vmu" (Proxy :: Proxy VMUChanged) $ readMVar . _vmu . fromObjRef,
        defSignalNamedParams "vmuError" (Proxy :: Proxy VMUError) (fstName "msg"),
        defSignalNamedParams "vmuInfo"  (Proxy :: Proxy VMUInfoMsg) (fstName "msg"),

        defMethod "createNewVMU" createNewVMU,
        defMethod "openVMU" openVMU,
        defMethod "saveVMU" saveVMU,
        defMethod "addVMUSaveFile" addVMUSaveFile,
        defMethod "saveVMUSaveFile" saveVMUSaveFile,
        defMethod "removeSaveFile" removeSaveFile,
        defMethod "unlockUnusedBlocks" unlockUnusedBlocks,
        defMethod "lockUnusedBlocks" lockUnusedBlocks]

instance DefaultClass VMU where
    classMembers = [
        defPropertyRO "files" $ refDirEntries . files . fromObjRef]

        where refDirEntries :: [Maybe DirectoryEntry] -> IO [ObjRef DirectoryEntry]
              refDirEntries entries = mapM newObjectDC (catMaybes entries)


instance DefaultClass DirectoryEntry where
    classMembers = [
        defPropertyRO "fileName" $ getStrProperty fileName,
        defPropertyRO "fileType" $ getStrProperty (show . fileType),
        defPropertyRO "blocks" $ getProperty (w16ToInt . sizeInBlocks),
        defPropertyRO "startBlock" $ getProperty (w16ToInt . startingBlock),
        defPropertyRO "timestamp" $ newObjectDC . timestamp . fromObjRef]
    
        where
          getStrProperty :: (DirectoryEntry -> String) -> ObjRef DirectoryEntry -> IO TX.Text 
          getStrProperty f = getProperty (TX.pack . f)  
          
          w16ToInt :: Word16 -> Int
          w16ToInt = fromIntegral 

          

instance DefaultClass Timestamp where
    classMembers = [
        defPropertyRO "century"   $ getTSProperty century,
        defPropertyRO "year"      $ getTSProperty year,
        defPropertyRO "month"     $ getTSProperty month,
        defPropertyRO "day"       $ getTSProperty day,
        defPropertyRO "hour"      $ getTSProperty hour,
        defPropertyRO "minute"    $ getTSProperty minute,
        defPropertyRO "second"    $ getTSProperty second,
        defPropertyRO "dayOfWeek" $ getTSProperty dayOfWeek]

        where
          -- Creation time is stored as a sequence of 16 bit BCD (Binary Coded
          -- Decimal) values, we convert this to the "proper"
          -- representation when obtaining the property 
          getTSProperty :: (Timestamp -> Word8) -> ObjRef Timestamp -> IO Int
          getTSProperty f = getProperty (fromBCD . f)

          fromBCD :: Word8 -> Int
          fromBCD n = ((leftDigit `mod` 10) * 10) + (rightDigit `mod` 10)
            where leftDigit = fromIntegral $ n `shiftR` 4
                  rightDigit = fromIntegral $ n .&. 0xF


------ ContextVMU methods -------

-- Create a newly formatted VMU
createNewVMU :: ObjRef ContextVMU -> IO ()
createNewVMU co = do
    modifyMVar_ vmu (\_ -> currentTimestamp >>= newObjectDC . newVMU)
    fireSignal (Proxy :: Proxy VMUChanged) co
  where 
        vmu = _vmu . fromObjRef $ co


-- Attempt to open + load VMU from disk
openVMU :: ObjRef ContextVMU -> TX.Text -> IO ()
openVMU co str = do 
    modifyMVar_ vmu (\vRef -> do

        fileBs <- BS.readFile fPath 
        case (createVMU fileBs) of
            Left err -> do 
                fireSignal (Proxy :: Proxy VMUError) co (TX.pack err)
                return vRef

            Right newVmu -> newObjectDC newVmu
        ) 
    -- TODO change so that this signal is only fired when open is
    -- successful
    fireSignal (Proxy :: Proxy VMUChanged) co
  
  where 
        vmu = _vmu . fromObjRef $ co
        fPath = TX.unpack str


-- Attempt to save VMU to disk
saveVMU :: ObjRef ContextVMU -> TX.Text -> IO ()
saveVMU co str = do 
    v <- readMVar vmu
    BS.writeFile fPath $ BS.pack $ exportVMU $ fromObjRef v
  where 
        vmu = _vmu . fromObjRef $ co
        fPath = TX.unpack str


addVMUSaveFile :: ObjRef ContextVMU -> TX.Text ->  IO ()
addVMUSaveFile co str = modifyMVar_ vmu (\vRef -> do
      fileBs <- BS.readFile fPath
      let v = fromObjRef vRef
      
      case (injectDCIFile (BS.unpack fileBs) v) of
        Left err -> do 
            fireSignal (Proxy :: Proxy VMUError) co (TX.pack err)
            return vRef

        Right v' -> do 
            fireSignal (Proxy :: Proxy VMUChanged) co 
            newObjectDC v'
    ) 

  where 
        vmu = _vmu . fromObjRef $ co
        fPath = TX.unpack str


-- Attempt to save specified VMU file to disk
saveVMUSaveFile :: ObjRef ContextVMU -> Int -> TX.Text -> IO ()
saveVMUSaveFile co fileNo str = do 
    v' <- readMVar vmu
    let v = fromObjRef v'

    -- TODO detect + support other formats
    case (extractDCIFromVMU v fileNo) of
         Left err ->  fireSignal (Proxy :: Proxy VMUError) co (TX.pack err)
  
         Right vmuFile -> BS.writeFile fPath $ BS.pack $ exportVMUFile vmuFile
  
  where 
        vmu = _vmu . fromObjRef $ co
        fPath = TX.unpack str


-- Remove a specified individual file from the VMU
removeSaveFile :: ObjRef ContextVMU -> Int -> IO ()
removeSaveFile co fileNo = modifyMVar_ vmu (\vRef -> do
      let v = fromObjRef vRef
      
      case (rmFromVMU fileNo v) of
        Left err -> do 
            fireSignal (Proxy :: Proxy VMUError) co (TX.pack err)
            return vRef

        Right v' -> do 
            fireSignal (Proxy :: Proxy VMUChanged) co 
            newObjectDC v'
    ) 

  where 
        vmu = _vmu . fromObjRef $ co


-- Unlock an extra unused 41 blocks of space
unlockUnusedBlocks :: ObjRef ContextVMU -> IO ()
unlockUnusedBlocks co = do
    vmu <- readMVar mVarVMU
    let currentBlocksUnlocked = userBlocksCount . root . fromObjRef $ vmu 
    
    -- Check if blocks are already unlocked
    if currentBlocksUnlocked == 241 
        then 
            fireSignal (Proxy :: Proxy VMUInfoMsg) co alreadyUnlockedMessage 
        else 
            case (unlockBlocksFromVMU $ fromObjRef vmu) of
                Left err -> fireSignal (Proxy :: Proxy VMUError) co (TX.pack err)
                Right v -> do 
                    modifyMVar_ mVarVMU (\_ -> newObjectDC v)
                    fireSignal (Proxy :: Proxy VMUChanged) co
  where 
    mVarVMU = _vmu . fromObjRef $ co
    alreadyUnlockedMessage = "Unused blocks are already unlocked." 


-- Lock the "extra" 41 blocks of space
-- TODO check if data stored in these blocks before locking
-- and display a confirmation dialog in this case
lockUnusedBlocks :: ObjRef ContextVMU -> IO ()
lockUnusedBlocks co = do
    vmu <- readMVar mVarVMU
    let currentBlocksUnlocked = userBlocksCount . root . fromObjRef $ vmu 
    
    -- Check if blocks are already locked
    if currentBlocksUnlocked == 200
        then 
            fireSignal (Proxy :: Proxy VMUInfoMsg) co alreadyUnlockedMessage 
        else 
            case (lockBlocksFromVMU $ fromObjRef vmu) of
                Left err -> fireSignal (Proxy :: Proxy VMUError) co (TX.pack err)
                Right v -> do 
                    modifyMVar_ mVarVMU (\_ -> newObjectDC v)
                    fireSignal (Proxy :: Proxy VMUChanged) co
  where 
    mVarVMU = _vmu . fromObjRef $ co
    alreadyUnlockedMessage = "Extra blocks are already locked."


-- Get a property out of an object reference and return as an IO action
getProperty :: (a -> b) -> ObjRef a -> IO b           
getProperty f = return . f . fromObjRef




main :: IO()
main = do
    curTime <- currentTimestamp
    vmu <- return $ newVMU curTime
    l <- newMVar =<< newObjectDC vmu -- Start off with empty VMU
    tc <- newObjectDC $ ContextVMU l

    runEngineLoop defaultEngineConfig {
      initialDocument = fileDocument "qml/potato_tool.qml"
    , contextObject = Just $ anyObjRef tc
    }
