-- Module for reading/extracting individual
-- Raw VMU files from the VMU

module VMUFile 
( VMUFile (..) 
, injectDCIFile
, injectRawFile
, exportVMUFile
, createVMUFileDCI
)
where

import VMU
import Data.Word
import Data.List.Split

data VMUFile = VMUFile 
    { fileInfo :: DirectoryEntry
    , blocks :: [[Word8]]
    }


-- DCI file format has bytes for each block reversed in groups of 4,
-- we need to reverse these bytes when importing and exporting
-- to read/write correctly to an ordinary Dreamcast VMU
reverseDCIBytes :: [Word8] -> [Word8]
reverseDCIBytes xs = concatMap reverse $ chunksOf 4 xs

injectDCIFile :: [Word8] -> VMU -> Either String VMU
injectDCIFile mem vmu = do
    file <- importRawVMUFile mem
    let dciFile = VMUFile (fileInfo file) (map reverseDCIBytes $ blocks file)
    injectVMUFile dciFile vmu
    
createVMUFileDCI :: DirectoryEntry -> [Word8] -> VMUFile
createVMUFileDCI entry raw = VMUFile entry $ chunksOf 512 $ reverseDCIBytes raw

injectRawFile :: [Word8] -> VMU -> Either String VMU
injectRawFile mem vmu = do
    file <- importRawVMUFile mem
    injectVMUFile file vmu

injectVMUFile :: VMUFile -> VMU -> Either String VMU
injectVMUFile file vmu = do 
    blockNos <- getNFreeBlocks (fromIntegral $ sizeInBlocks $ fileInfo file) vmu
    let dirEntry = DirectoryEntry ft cp (head blockNos)  fn ts sib oib
    newFiles <- insertDirEntry (files vmu) dirEntry
    let newUserBlocks = insertBlocks blockNos (blocks file) (userBlocks vmu)
    let newFAT = insertFAT blockNos (fat vmu)
    return $ VMU (root vmu) newFiles newFAT newUserBlocks (unused vmu)
        where oldDirEntry = fileInfo file
              ft = fileType oldDirEntry
              cp = copyProtected oldDirEntry
              fn = fileName oldDirEntry
              ts = timestamp oldDirEntry
              sib = sizeInBlocks oldDirEntry
              oib = offsetInBlocks oldDirEntry  
              

importRawVMUFile :: [Word8] -> Either String VMUFile
importRawVMUFile mem 
    | length mem < 32 = Left ("File is too small" ++ 
        show (length mem) ++ "bytes")
    | (length mem - 32) `mod` 512 /= 0 = Left ("File doesn't contain" ++
        " full blocks")
    | otherwise = either Left (checkSize mem) $ getDirEntry mem


checkSize :: [Word8] -> DirectoryEntry -> Either String VMUFile
checkSize mem entry = 
    if specifiedBlocks == actualBlocks
        then Right $ VMUFile entry $ chunksOf 512 $ drop 32 mem
        else Left ("File directory info specifies it contains " ++ 
            show specifiedBlocks ++ "blocks however it actually contains" ++ 
            show actualBlocks ++ "blocks")

    where actualBlocks = (length mem - 32) `div` 512
          specifiedBlocks = fromIntegral $ sizeInBlocks entry  


exportVMUFile :: VMUFile -> [Word8]
exportVMUFile v = 
    (exportDirEntry . Just . fileInfo) v ++ (concat . blocks) v
