-- Module for reading/extracting individual
-- Raw VMU files from the VMU

module RawVMUFile 
( VMUFile (..) 
, injectDCIFile
, injectRawFile
, exportVMUFile
)
where

import VMU
import Data.Word
import Data.Binary
import Data.Bits
import Data.List.Split

data VMUFile = VMUFile 
    { fileInfo :: DirectoryEntry
    , blocks :: [[Word8]]
    }


-- DCI file format has bytes for each block reversed in groups of 4,
-- we need to reverse these bytes when importing and exporting
-- to read/write correctly to an ordinary Dreamcast VMU
reverseDCIBytes :: [Word8] -> [Word8]
reverseDCIBytes xs = concatMap (reverse) $ chunksOf 4 xs

injectDCIFile :: [Word8] -> VMU -> Either String VMU
injectDCIFile mem vmu = do
    file <- importRawVMUFile mem
    let dciFile = VMUFile (fileInfo file) (map reverseDCIBytes $ blocks file)
    injectVMUFile dciFile vmu
    

injectRawFile :: [Word8] -> VMU -> Either String VMU
injectRawFile mem vmu = do
    file <- importRawVMUFile mem
    injectVMUFile file vmu

injectVMUFile :: VMUFile -> VMU -> Either String VMU
injectVMUFile file vmu = do 
    blockNos <- getNFreeBlocks (fromIntegral $ sizeInBlocks $ fileInfo file) vmu
    newFiles <- insertDirEntry (files vmu) (fileInfo file)
    let newUserBlocks = insertBlocks blockNos (blocks file) (userBlocks vmu)
    let newFAT = insertFAT blockNos (fat vmu)
    return $ VMU (root vmu) newFiles newFAT newUserBlocks


importRawVMUFile :: [Word8] -> Either String VMUFile
importRawVMUFile mem 
    | length mem < 32 = Left ("File is too small" ++ 
        (show $ length mem) ++ "bytes")
    | (length mem - 32) `mod` 512 /= 0 = Left ("File doesn't contain" ++
        " full blocks")
    | otherwise = either (Left) (checkSize mem) $ getDirEntry mem


checkSize :: [Word8] -> DirectoryEntry -> Either String VMUFile
checkSize mem entry = 
    if specifiedBlocks == actualBlocks
        then Right $ VMUFile entry $ chunksOf 512 $ drop 32 mem
        else Left ("File directory info specifies it contains " ++ 
            (show specifiedBlocks) ++ "blocks however it actually contains" ++ 
            (show actualBlocks) ++ "blocks")

    where actualBlocks = ((length mem) - 32) `div` 512
          specifiedBlocks = fromIntegral $ sizeInBlocks entry  


exportVMUFile :: VMUFile -> [Word8]
exportVMUFile v = 
    [fileTypeMem] ++  [protectedMem] ++ startBlocks ++ fileNameMem ++
    timeStampMem ++ blockSizeMem ++ headerOffset ++ blocksMem
                    
    where 
        blocksMem = concat $ blocks v
        fileInf = fileInfo v
        fileTypeMem = case fileType fileInf of
            Data -> 0x33 
            Game -> 0xCC 
        protectedMem = case copyProtected fileInf of
            False -> 0x00 
            True  -> 0xFF 
        startBlocks = splitW16Le $ startingBlock fileInf  
        timeStampMem = exportTimestamp $ timestamp fileInf
        fileNameMem = map (fromIntegral . fromEnum) $ take 11 (fileName fileInf) 
        blockSizeMem = splitW16Le $ sizeInBlocks fileInf
        headerOffset = splitW16Le $ offsetInBlocks fileInf   


exportTimestamp :: Timestamp -> [Word8]
exportTimestamp ts = ct ++ yr ++ mnth ++ dy ++ hr ++ min ++ sec ++ dow
    where 
        ct   = [century ts]
        yr   = [year ts]
        mnth = [month ts]
        dy   = [day ts]
        hr   = [hour ts]
        min  = [minute ts]
        sec  = [second ts]
        dow  = [dayOfWeek ts]

-- Split a Word 16 into two Word 8s,
-- the first one being the lower byte and the second
-- entry being the higher byte
splitW16Le :: Word16 -> [Word8] 
splitW16Le num = [n `shiftR` 8] ++ [n .&. 0xFF]
    where n = fromIntegral num
