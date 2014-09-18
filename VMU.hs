module VMU 
( VMU(files) 
, DirectoryEntry 
    ( fileType
    , copyProtected
    , startingBlock
    , fileName
    , timestamp
    , sizeInBlocks
    , offsetInBlocks
    )
, FileType (Game, Data)
, Timestamp 
    ( century
    , year
    , month
    , day
    , hour
    , minute
    , second
    , dayOfWeek
    )
, rawDumpFile 
, getBlocks
, getFreeBlocks
, getNFreeBlocks
, createVMU
, getDirEntry
) where 

import Data.Word
import Data.Binary
import Data.Maybe
import Data.Char
import Data.Either
import Data.List.Split
import qualified Data.ByteString.Lazy as BS
import Data.Bits
import Control.Applicative

data VMU = VMU
    { root :: RootBlock
    , files :: [Maybe DirectoryEntry]
    , fat :: [Word16]
    , userBlocks   :: [[Word8]]   
    }


data Timestamp = Timestamp
    { century :: Word8
    , year :: Word8
    , month :: Word8
    , day :: Word8
    , hour :: Word8
    , minute :: Word8
    , second :: Word8
    , dayOfWeek :: Word8
    } deriving Show
 

data RootBlock = RootBlock
    { customVMSColor :: Word8 -- Bool
    , blueVMS :: Word8
    , redVMS :: Word8
    , greenVMS :: Word8
    , alphaComponent :: Word8
    , timeStamp :: Timestamp
    , locationFAT :: Word16
    , sizeFAT :: Word16
    , locationDirectory :: Word16
    , sizeDirectory :: Word16
    , iconShape :: Word16
    , userBlocksCount :: Word16
    } deriving Show


data DirectoryEntry = DirectoryEntry 
    { fileType :: FileType
    , copyProtected :: Bool
    , startingBlock :: Word16
    , fileName :: String
    , timestamp :: Timestamp
    , sizeInBlocks :: Word16
    , offsetInBlocks :: Word16
    } deriving Show

data FileType = Game | Data deriving Show

vmuSize = 128 * 1024 --128KB vmu

slice :: Int -> Int -> [Word8] -> [Word8] 
slice a b xs = take (b - a + 1) (drop a xs)

-- Work out starting location in file for given block
blockStart :: Int -> Int
blockStart b = 512 * b

-- Concatonate two Word8 values into a little
-- endian Word16
encodeWord16 :: [Word8] -> Word16
encodeWord16 (a:b:xs) = a' .|. (b' `shiftL` 8)
    where a' = fromIntegral a
          b' = fromIntegral b

-- Obtain file information in the directory
-- from given file no
getEntry :: Int -> VMU -> Maybe DirectoryEntry
getEntry fileNo vmu  
    | fileNo >= (length . files) vmu = Nothing
    | otherwise = Just $ (catMaybes $ files vmu) !! fileNo

-- Obtain the first N free blocks, starting from the highest
-- block, returns a list of free blocks the size requested if
-- available, otherwise returns an error message
getNFreeBlocks :: Int -> VMU -> Either String [Word16]
getNFreeBlocks n vmu 
    | length unallocBlockNos < n = Left 
            ((show n) ++ "free blocks required, there are only " ++
             (show $ length unallocBlockNos) ++ "free blocks available")
    | otherwise = Right $ take n unallocBlockNos
    where 
        unallocBlockNos = filter (== 0xFFFC) $ reverse $ take highestBlock fatMem
        fatMem = fat vmu
        highestBlock = fromIntegral $ userBlocksCount $ root vmu


-- Obtain block numbers for given file
getBlocks :: Int -> VMU -> Maybe [Word16]
getBlocks fileNo vmu 
    | fileNo >= (length . files) vmu = Nothing 
    | otherwise = Just $ getBlocks' ((catMaybes $ files vmu) !! fileNo) (fat vmu)

getBlocks' :: DirectoryEntry -> [Word16] -> [Word16]
getBlocks' file fatMem = getBlocks'' (startingBlock file) fatMem
   

getBlocks'' :: Word16 -> [Word16] -> [Word16]
getBlocks'' blockNo fatMem  
    | nextBlock == 0xFFFA  = [blockNo]
    | nextBlock <= 0xFF    = blockNo : getBlocks'' nextBlock fatMem 
    | nextBlock == 0xFFFC  = error ("Block " ++ (show blockNo) ++ 
        "is unallocated")
     
    | otherwise = error ("FileSystem is corrupt, block" ++ (show blockNo) ++
        "contains an invalid value " ++ (show nextBlock))

        where nextBlock = fatMem !! fromIntegral blockNo

-- Attempt to insert a directory entry into
-- the VMU directory in the first empty spot,
-- returns modified directory if successful
insertDirEntry :: [Maybe DirectoryEntry] ->
                  DirectoryEntry ->
                  Either String [Maybe DirectoryEntry]
insertDirEntry dir entry
    | null ys  = Left "No directory space left for new entry"
    | otherwise = Right $ xs ++ [Just entry] ++ (tail ys) 
    where xs = takeWhile (isJust) dir
          ys = dropWhile (isJust) dir


-- Obtain the number of free block available on the VMU
getFreeBlocks :: VMU -> Word16
getFreeBlocks vmu = 
    fromIntegral $ length $ filter (== 0xFFFC) $ take blockCount fatMem
    where 
        blockCount = fromIntegral $ userBlocksCount $ root vmu
        fatMem = fat vmu

-- Obtain a raw dump for a given file in the filesystem
rawDumpFile :: Int -> VMU -> Maybe [Word8]
rawDumpFile fileNo vmu = do 
    let blockMem = userBlocks vmu
    fileInfo <- getEntry fileNo vmu 
    let blockNos = getBlocks' fileInfo (fat vmu)
    return $ concatMap (\b -> blockMem !! fromIntegral b) blockNos


-- Obtain the last block Root Block so we can determine
-- information on the file system to operate on it
createRootBlock :: [Word8] -> RootBlock
createRootBlock fileStr = 
        RootBlock customColor blue red green alpha timeS
                  locationFAT sizeFAT locationDir 
                  sizeDir iconShape userBlocksCount
        
        where 
              rootBlockStr = drop (blockStart 255) fileStr
              customColor = rootBlockStr !! 0x10 -- /= 0x0
              blue = rootBlockStr !! 0x11
              green = rootBlockStr !! 0x12
              red = rootBlockStr !! 0x13
              alpha = rootBlockStr !! 0x14
              timeS = createTimestamp $ drop 0x29 rootBlockStr
              locationFAT = encodeWord16 $ slice 0x46 0x47 rootBlockStr
              sizeFAT = encodeWord16 $ slice 0x48 0x49 rootBlockStr
              locationDir = encodeWord16 $ slice 0x4A 0x4B rootBlockStr
              sizeDir = encodeWord16 $ slice 0x4C 0x4D rootBlockStr
              iconShape = encodeWord16 $ slice 0x4E 0x4F rootBlockStr
              userBlocksCount = encodeWord16 $ slice 0x50 0x51 rootBlockStr


-- Obtain Timestamp
createTimestamp :: [Word8] -> Timestamp
createTimestamp mem =
    Timestamp cen yr mnth day hr min sec dow
    
    where 
        cen  = mem !! 0
        yr   = mem !! 1  
        mnth = mem !! 2
        day  = mem !! 3
        hr   = mem !! 4
        min  = mem !! 5
        sec  = mem !! 6
        dow  = mem !! 7  

-- Read 32 Bytes entry into a Directory Entry
-- If file type is none or unrecognized value is read in
-- then Nothing is returned as it is not a valid directory entry
-- TODO possibly distinguish between corrupt and no file
getDirEntry :: [Word8] -> Either String DirectoryEntry
getDirEntry entry = 
    DirectoryEntry <$> fType <*> protected <*> startingB <*> 
        name <*> timeS <*> sizeB <*> offsetB 

    where 
        fType = case entry !! 0x0 of
                0x33 -> Right Data
                0xCC -> Right Game
                0x00 -> Left "File marked as empty"
                otherwise -> Left ("Unknown type value" ++ (show entry))
    
        protected = case entry !! 0x1 of
                0x00 -> Right False
                0xFF -> Right True
                otherwise -> Left ("Unknown protected value" ++ (show entry))

        startingB = Right $ encodeWord16 $ slice 0x2 0x3 entry
        name = Right $ map (chr . fromEnum) $ slice 0x4 0xF entry
        timeS = Right $ createTimestamp $ drop 0x9 entry
        sizeB = Right $ encodeWord16 $ slice 0x18 0x19 entry
        offsetB = Right $ encodeWord16 $ slice 0x1A 0x1B entry


createDirectory :: RootBlock -> [Word8] -> [Maybe DirectoryEntry]
createDirectory rb vmu = map (either (\_  -> Nothing) Just) entries
    where dirBlockStart = fromIntegral $ locationDirectory rb
          noBlocks = fromIntegral $ sizeDirectory rb
          dirSizeBytes =  blockStart $ fromIntegral noBlocks

          entries = 
            concatMap (entriesBlock) [dirBlockStart,dirBlockStart-1..dirBlockStart - noBlocks]

          -- 16 32 bytes entries in 512 byte block
          entriesBlock n = [getDirEntry $ 
            slice ((blockStart n) + x * 32) ((blockStart n) - 1 + ((x + 1) * 32)) vmu | 
                x <- [0..15]] 

createUserBlocks :: RootBlock -> [Word8] -> [[Word8]]
createUserBlocks rb mem = chunksOf 512 $ take (noBlocks * 512) mem
    where noBlocks = fromIntegral $ userBlocksCount rb 

createFAT :: RootBlock -> [Word8] -> [Word16]
createFAT rb mem =   map (encodeWord16) $ chunksOf 2 fatMem8
    where noBlocks = fromIntegral $ sizeFAT rb
          startFAT = blockStart $ fromIntegral $ locationFAT rb
          fatMem8 = (take (512 * noBlocks)) $ (drop startFAT) mem   
            

createVMU :: BS.ByteString -> Either String VMU
createVMU bs 
    | BS.length bs /= vmuSize = Left ("VMU is incorrect size (" ++ 
        (show $ BS.length bs) ++ " bytes) should be exactly " ++ (show vmuSize) ++ 
        "bytes")
    | otherwise = Right $ VMU rb dirs fat blocks
        where 
              mem = BS.unpack bs
              rb = createRootBlock mem
              dirs = createDirectory rb mem
              fat = createFAT rb mem 
              blocks = createUserBlocks rb mem
