module Operations (rm, injectDCI, extractDCI, unlockBlocks) where

import Text.Printf
import qualified Data.ByteString.Lazy as BS
import VMU
import VMUFile

rm :: Int -> BS.ByteString -> Either String VMU
rm fileNo vmuBs = do
    vmu <- createVMU vmuBs
    clearFile fileNo vmu



-- |Inject a nexus DCI format save file into the filesystem
injectDCI :: BS.ByteString -> BS.ByteString -> Either String VMU
injectDCI vmuBs file = do
    vmu <- createVMU vmuBs
    injectDCIFile (BS.unpack file) vmu


-- |Extract a file from the filesystem in the nexus DCI format
extractDCI :: Int -> BS.ByteString -> Either String VMUFile
extractDCI fileNo vmuBs = do
    vmu <- createVMU vmuBs
    fInfo <- getEntry fileNo vmu
    fileRaw <- rawDumpFile fileNo vmu
    return $ createVMUFileDCI fInfo fileRaw


-- |Unlock unused blocks 200 - 240 in the filesystem for use
unlockBlocks :: BS.ByteString -> Either String VMU
unlockBlocks vmuBs = do
    vmu <- createVMU vmuBs
    let rootBlock = root vmu
    let newRoot = rootBlock {userBlocksCount = 241}
    return vmu {root = newRoot}


