module Operations where

import Text.Printf()
import qualified Data.ByteString.Lazy as BS
import VMU
import VMUFile


-- |Remove a file from the filesystem
rmFromVMU :: Int -> VMU -> Either String VMU
rmFromVMU = clearFile

rm :: Int -> BS.ByteString -> Either String VMU
rm fileNo vmuBs = do
    vmu <- createVMU vmuBs
    rmFromVMU fileNo vmu


-- |Inject a nexus DCI format save file into the filesystem
injectDCI :: BS.ByteString -> BS.ByteString -> Either String VMU
injectDCI vmuBs file = do
    vmu <- createVMU vmuBs
    injectDCIFile (BS.unpack file) vmu


-- |Extract a file from the filesystem in the nexus DCI format
extractDCIFromVMU :: VMU -> Int -> Either String VMUFile
extractDCIFromVMU vmu fileNo = do
    fInfo <- getEntry fileNo vmu
    fileRaw <- rawDumpFile fileNo vmu
    return $ createVMUFileDCI fInfo fileRaw
    

extractDCI :: Int -> BS.ByteString -> Either String VMUFile
extractDCI fileNo vmuBs = do
    vmu <- createVMU vmuBs
    extractDCIFromVMU vmu fileNo


-- |Unlock unused blocks 200 - 240 in the filesystem for use
unlockBlocksFromVMU :: VMU -> Either String VMU
unlockBlocksFromVMU vmu = do
    let rootBlock = root vmu
    let newRoot = rootBlock {userBlocksCount = 241}
    return vmu {root = newRoot}


unlockBlocks :: BS.ByteString -> Either String VMU
unlockBlocks vmuBs = do
    vmu <- createVMU vmuBs
    unlockBlocksFromVMU vmu


-- |Lock blocks 200 - 240 in the filesystem
lockBlocksFromVMU :: VMU -> Either String VMU
lockBlocksFromVMU vmu = do
    let rootBlock = root vmu
    let newRoot = rootBlock {userBlocksCount = 200}
    return vmu {root = newRoot}
 

lockBlocks :: BS.ByteString -> Either String VMU
lockBlocks vmuBs = do
    vmu <- createVMU vmuBs
    unlockBlocksFromVMU vmu
