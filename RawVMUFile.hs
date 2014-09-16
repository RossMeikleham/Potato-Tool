-- Module for reading/extracting individual
-- Raw VMU files from the VMU

module RawVMUFile where

import VMU
import Data.Word
import Data.List.Split

data RawVMUFile = RawVMUFile 
    { fileInfo :: DirectoryEntry
    , blocks :: [[Word8]]
    }

importRawVMUFile :: [Word8] -> Either String RawVMUFile
importRawVMUFile mem 
    | length mem < 32 = Left ("File is too small" ++ 
        (show $ length mem) ++ "bytes")
    | (length mem - 32) `mod` 512 /= 0 = Left ("File doesn't contain" ++
        " full blocks")
    | otherwise = either (Left) (checkSize mem) $ getDirEntry mem

checkSize :: [Word8] -> DirectoryEntry -> Either String RawVMUFile
checkSize mem entry = 
    if specifiedBlocks == actualBlocks
        then Right $ RawVMUFile entry $ chunksOf 512 $ drop 32 mem
        else Left ("File directory info specifies it contains " ++ 
            (show specifiedBlocks) ++ "blocks however it actually contains" ++ 
            (show actualBlocks) ++ "blocks")

    where actualBlocks = ((length mem) - 32) `div` 512
          specifiedBlocks = fromIntegral $ sizeInBlocks entry  

exportVMUFileToRaw :: RawVMUFile -> [Word8]
exportVMUFileToRaw v = 
    where blocksMem = concat $ blocks v
          fileInfoMem = 
