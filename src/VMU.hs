module VMU where

import           Data.Bits
import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.List.Split
import           Data.Maybe
import           Data.Word
import           Data.Typeable

-- | Dreamcast VMU filesystem is made up of 255 blocks, each block contains 
--   512 bytes of data. The blocks in the filesystem are of the following  
--   structure:
--   
--  Root block: 255th block: 
--  Contains "core" info on the filesystem such as locations and 
--  sizes of FAT and directory system files.
--
--  Unused blocks (200 - 240): 
--  These blocks can be freed for use as user data blocks.
--
--  FAT block 254th block: 
--  Works similar to a MS-DOS FAT16 File Allocation Table. 
--
--  Directory blocks (241 - 253):
--  The Directory lists all the user files stored in the VMS. The Directory
--  consists of a sequence of 32-byte entries each potentially describing
--  a file. 
--
--  User Data blocks (0 - 239):
--  Contains binary file data
data VMU = VMU
    { root       :: RootBlock 
    , files      :: [Maybe DirectoryEntry]
    , fat        :: [Word16]
    , userBlocks :: [[Word8]]
    , unused     :: [Word8] -- blocks 200 - 240 are unused by default
    } deriving (Show, Typeable)


-- | Dreamcast timestamp
data Timestamp = Timestamp
    { century   :: Word8
    , year      :: Word8
    , month     :: Word8
    , day       :: Word8
    , hour      :: Word8
    , minute    :: Word8
    , second    :: Word8
    , dayOfWeek :: Word8 -- 0 (Monday) - 6 (Sunday)
    } deriving (Show, Typeable)


-- | Data on the Root Block
data RootBlock = RootBlock
    { customVMSColor    :: Bool -- 0x010
    , blueVMS           :: Word8 -- 0x011
    , redVMS            :: Word8 -- 0x012
    , greenVMS          :: Word8 -- 0x013
    , alphaComponent    :: Word8 -- 0x014
    , timeStamp         :: Timestamp -- 0x030 -0x037
    , locationFAT       :: Word16 -- 0x046 - 0x047
    , sizeFAT           :: Word16 -- 0x048 - 0x049
    , locationDirectory :: Word16 -- 0x04A - 0x04B
    , sizeDirectory     :: Word16 -- 0x04C - 0x04D
    , iconShape         :: Word16 -- 0x04E - 0x04F
    , userBlocksCount   :: Word16 -- 0x050 - 0x051
    , unknownValues1    :: [Word8] -- 0x40 - 0x45
    , unknownValues2    :: [Word8] -- 0x52 - 0x1FF
    } deriving (Show, Typeable)


-- | Data for each Directory Entry block
data DirectoryEntry = DirectoryEntry
    { fileType       :: FileType -- 0x00
    , copyProtected  :: Bool -- 0x01
    , startingBlock  :: Word16 -- 0x02 - 0x03
    , fileName       :: String -- 0x04 - 0x0F
    , timestamp      :: Timestamp -- 0x10 - 0x17
    , sizeInBlocks   :: Word16 -- 0x18 - 0x19
    , offsetInBlocks :: Word16 -- 0x1A - 0x1B
    } deriving (Show, Typeable)


-- | Files can either be Game files which are standalone
--   VMU games, or data files typically storing data
--   from Dreamcast games
data FileType = Game | Data deriving (Show, Typeable)


vmuSize :: Int
vmuSize = 128 * 1024 --128KB vmu


int :: Integral a => a -> Int
int x = fromIntegral x :: Int


slice :: Int -> Int -> [Word8] -> [Word8]
slice a b xs = take (b - a + 1) (drop a xs)


-- | Work out starting location in file for given block
blockStart :: Int -> Int
blockStart b = 512 * b


-- |Concatonate two Word8 values into a little
--  endian Word16
encodeWord16 :: [Word8] -> Word16
encodeWord16 (a:b:_) = a' .|. (b' `shiftL` 8)
    where a' = fromIntegral a
          b' = fromIntegral b
encodeWord16 _ = error "Need 2 Word8s"


-- |Split a Word 16 into two Word 8s,
-- the first one being the lower byte and the second
-- entry being the higher byte
splitW16Le :: Word16 -> [Word8]
splitW16Le num = map fromIntegral $ (num .&. 0xFF) : [num `shiftR` 8]


-- |Obtain file information in the directory
-- from given file no
getEntry :: Int -> VMU -> Either String DirectoryEntry
getEntry fileNo vmu
    | fileNo > (length . catMaybes . files) vmu = Left $ "file number " ++
        show fileNo ++ " doesn't exist, use the \"ls\" command to obtain" ++
        "valid files in the vmu"
    | otherwise = Right $ catMaybes (files vmu) !! (fileNo - 1)


-- |Obtain the first N free blocks, starting from the highest
-- block, returns a list of free blocks the size requested if
-- available, otherwise returns an error message
getNFreeBlocks :: Int -> VMU -> Either String [Word16]
getNFreeBlocks n vmu
    | length unallocBlockNos < n = Left
            (show n ++ "free blocks required, there are only " ++
             show (length unallocBlockNos) ++ "free blocks available")
    | otherwise = Right $ map fst $ take n unallocBlockNos
    where
        unallocBlockNos = filter (\(_, x) -> x == 0xFFFC) $ reverse $ take highestBlock fatMem
        fatMem = toIndicies 0 $ fat vmu
        highestBlock = fromIntegral $ userBlocksCount $ root vmu


toIndicies :: Word16 -> [a] -> [(Word16, a)]
toIndicies _ [] = []
toIndicies i (x:xs)  = (i , x) : toIndicies (i + 1) xs


-- |Obtain block numbers for given file
getBlocks :: Int -> VMU -> Maybe [Word16]
getBlocks fileNo vmu
    | fileNo >= (length . files) vmu = Nothing
    | otherwise = Just $ getBlocks' (catMaybes (files vmu) !! fileNo) (fat vmu)

getBlocks' :: DirectoryEntry -> [Word16] -> [Word16]
getBlocks' file = getBlocks'' (startingBlock file)


getBlocks'' :: Word16 -> [Word16] -> [Word16]
getBlocks'' blockNo fatMem
    | nextBlock == 0xFFFA  = [blockNo]
    | nextBlock <= 0xFF    = blockNo : getBlocks'' nextBlock fatMem
    | nextBlock == 0xFFFC  = error ("Block " ++ show blockNo ++
        "is unallocated")

    | otherwise = error ("FileSystem is corrupt, block" ++ show blockNo ++
        "contains an invalid value " ++ show nextBlock)

        where nextBlock = fatMem !! fromIntegral blockNo

-- |Insert multiple blocks with their given position into the
--user blocks
insertBlocks :: [Word16] -> [[Word8]] -> [[Word8]] -> [[Word8]]
insertBlocks blockNos newBlocks curBlocks =
    foldl (\c (x,y) -> insertBlock x y c) curBlocks $ zip bNos newBlocks
    where
        bNos = map fromIntegral blockNos

-- | Insert a single block into the given position of total blocks
insertBlock :: Int -> [Word8] -> [[Word8]] -> [[Word8]]
insertBlock blockNo newBlock oldBlocks =
    take blockNo oldBlocks ++ [newBlock] ++ drop (blockNo + 1) oldBlocks


-- | Attempt to insert a directory entry into
--   the VMU directory in the first empty spot,
--   returns modified directory if successful
insertDirEntry :: [Maybe DirectoryEntry] ->
                  DirectoryEntry ->
                  Either String [Maybe DirectoryEntry]
insertDirEntry dir entry
    | null ys  = Left "No directory space left for new entry"
    | otherwise = Right $ xs ++ [Just entry] ++ tail ys
    where xs = takeWhile isJust dir
          ys = dropWhile isJust dir


eraseDirEntry :: [Maybe DirectoryEntry] ->
                 Int ->
                 Either String [Maybe DirectoryEntry]
eraseDirEntry [] _ = Left "File doesn't exist"
eraseDirEntry (Just _:xs) 1 = Right (Nothing : xs)
eraseDirEntry (Just x:xs) n = (++) <$> Right [Just x] <*> eraseDirEntry xs (n - 1)
eraseDirEntry (Nothing:xs) n = (++) <$> Right [Nothing] <*> eraseDirEntry xs n


-- |Update the FAT for new blocks for a file
insertFAT :: [Word16] -> [Word16] -> [Word16]
insertFAT [] f = f
insertFAT (x:[]) f = insertValFAT x 0xFFFA f
insertFAT (x:y:xs) f = insertFAT (y:xs) $ insertValFAT x y f


-- |Marks blocks as unallocated in FAT
removeFAT :: [Word16] -> [Word16] -> [Word16]
removeFAT xs f = foldl (\ s x -> insertValFAT x 0xFFFC s) f xs

insertValFAT :: Word16 -> Word16 -> [Word16] -> [Word16]
insertValFAT x y f = take (int x) f ++ [y] ++ drop (int x + 1) f


-- |Obtain the number of free block available on the VMU
getFreeBlocks :: VMU -> Word16
getFreeBlocks vmu =
    fromIntegral $ length $ filter (== 0xFFFC) $ take blockCount fatMem
    where
        blockCount = fromIntegral $ userBlocksCount $ root vmu
        fatMem = fat vmu

-- |Obtain a raw dump for a given file in the filesystem
rawDumpFile :: Int -> VMU -> Either String [Word8]
rawDumpFile fileNo vmu = do
    let blockMem = userBlocks vmu
    fileInfo <- getEntry fileNo vmu
    let blockNos = getBlocks' fileInfo (fat vmu)
    return $ concatMap (\b -> blockMem !! fromIntegral b) blockNos


-- |Clear the numbered file from the filesystem
clearFile :: Int -> VMU -> Either String VMU
clearFile fileNo vmu = do
    let blockMem = userBlocks vmu
    fileInfo <- getEntry fileNo vmu
    let blockNos = getBlocks' fileInfo (fat vmu)
        newBlocks = insertBlocks blockNos (take (length blockNos) zeroBlocks) blockMem
        zeroBlocks = repeat $ take 512 [0,0..]
        newFat = removeFAT blockNos $ fat vmu
    newDir <- eraseDirEntry (files vmu) fileNo
    return $ vmu {files = newDir, userBlocks = newBlocks, fat = newFat}


-- |Obtain the last block Root Block so we can determine
-- information on the file system to operate on it
createRootBlock :: [Word8] -> RootBlock
createRootBlock fileStr =
        RootBlock customColor blue red green alpha timeS
                  lFAT sFAT lDir
                  sDir iShape uBlocksCount
                  unknown1 unknown2

        where
              rootBlockStr = drop (blockStart 255) fileStr
              customColor = rootBlockStr !! 0x10 /= 0x0
              blue = rootBlockStr !! 0x11
              green = rootBlockStr !! 0x12
              red = rootBlockStr !! 0x13
              alpha = rootBlockStr !! 0x14
              timeS = createTimestamp $ drop 0x30 rootBlockStr
              lFAT = encodeWord16 $ slice 0x46 0x47 rootBlockStr
              sFAT = encodeWord16 $ slice 0x48 0x49 rootBlockStr
              lDir = encodeWord16 $ slice 0x4A 0x4B rootBlockStr
              sDir = encodeWord16 $ slice 0x4C 0x4D rootBlockStr
              iShape = encodeWord16 $ slice 0x4E 0x4F rootBlockStr
              uBlocksCount = encodeWord16 $ slice 0x50 0x51 rootBlockStr
              unknown1 = slice 0x40 0x45 rootBlockStr
              unknown2 = slice 0x52 0x1FF rootBlockStr


-- |Obtain Timestamp
createTimestamp :: [Word8] -> Timestamp
createTimestamp mem =
    Timestamp cen yr mnth d hr m sec dow

    where
        cen  = head mem
        yr   = mem !! 1
        mnth = mem !! 2
        d    = mem !! 3
        hr   = mem !! 4
        m    = mem !! 5
        sec  = mem !! 6
        dow  = mem !! 7

-- |Read 32 Bytes entry into a Directory Entry
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
                _ -> Left ("Unknown type value" ++ show entry)

        protected = case entry !! 0x1 of
                0x00 -> Right False
                0xFF -> Right True
                _ -> Left ("Unknown protected value" ++ show entry)

        startingB = Right $ encodeWord16 $ slice 0x2 0x3 entry
        name = Right $ map (chr . fromEnum) $ slice 0x4 0xF entry
        timeS = Right $ createTimestamp $ drop 0x10 entry
        sizeB = Right $ encodeWord16 $ slice 0x18 0x19 entry
        offsetB = Right $ encodeWord16 $ slice 0x1A 0x1B entry


createDirectory :: RootBlock -> [Word8] -> [Maybe DirectoryEntry]
createDirectory rb vmu = map (either (const Nothing) Just) entries
    where dirBlockStart = fromIntegral $ locationDirectory rb
          noBlocks = fromIntegral $ sizeDirectory rb
          entries =
            concatMap entriesBlock [dirBlockStart,dirBlockStart-1..dirBlockStart - (noBlocks -1)]

          -- 16 32 bytes entries in 512 byte block
          entriesBlock n = [getDirEntry $
            slice (blockStart n + x * 32) (blockStart n - 1 + ((x + 1) * 32)) vmu |
                x <- [0..15]]

createUserBlocks :: RootBlock -> [Word8] -> [[Word8]]
createUserBlocks rb mem = chunksOf 512 $ take (noBlocks * 512) mem
    where noBlocks = fromIntegral $ userBlocksCount rb

createFAT :: RootBlock -> [Word8] -> [Word16]
createFAT rb mem =   map encodeWord16 $ chunksOf 2 fatMem8
    where noBlocks = fromIntegral $ sizeFAT rb
          startFAT = blockStart $ fromIntegral $ locationFAT rb
          fatMem8 = take (512 * noBlocks) $ drop startFAT mem


createVMU :: BS.ByteString -> Either String VMU
createVMU bs
    | (int . BS.length) bs /= vmuSize = Left ("VMU is incorrect size (" ++
        show (BS.length bs) ++ " bytes) should be exactly " ++ show vmuSize ++
        "bytes")
    | otherwise = Right $ VMU rb dirs f blocks extraBlocks
        where
              mem = BS.unpack bs
              rb = createRootBlock mem
              dirs = createDirectory rb mem
              f = createFAT rb mem
              blocks = createUserBlocks rb mem
              extraBlocks = take (blockStart $ int leftOverBC) $
                drop (blockStart $ int $  userBlocksCount rb) mem
              leftOverBC =  241 - userBlocksCount rb


exportVMU :: VMU -> [Word8]
exportVMU vmu = ub ++ eb ++ dirs ++ ft ++ rb
    where
          ub = concat (userBlocks vmu)
          eb = unused vmu
          ft = concatMap splitW16Le (fat vmu)
          dirs = concat $ reverse $ chunksOf 512 $ concatMap exportDirEntry (files vmu)
          rb = exportRootBlock (root vmu)

exportDirEntry :: Maybe DirectoryEntry -> [Word8]
exportDirEntry dir = case dir of
    Nothing -> take 32 [0,0..]
    Just d ->  [fileTypeMem] ++  [protectedMem] ++ startBlocks ++
                fileNameMem ++ timeStampMem ++ blockSizeMem ++ headerOffset ++
                notUsed

        where
            fileTypeMem = case fileType d of
                Data -> 0x33
                Game -> 0xCC
            protectedMem = if copyProtected d then 0xFF else 0x00
            startBlocks = splitW16Le $ startingBlock d
            timeStampMem = exportTimestamp $ timestamp d
            fileNameMem = map (fromIntegral . fromEnum) $ take 12 (fileName d)
            blockSizeMem = splitW16Le $ sizeInBlocks d
            headerOffset = splitW16Le $ offsetInBlocks d
            notUsed = take 4 [0x0,0x0..]


exportRootBlock :: RootBlock -> [Word8]
exportRootBlock rb = start ++ custom ++ blue ++ green ++
    red ++ alpha ++ padding ++ ts ++ padding2 ++ unknown1 ++
    fatLoc ++ fatSize ++ dirLoc ++ dirSize ++ is ++ ubc ++ unknown2

    where
        start = take 0x10 [0x55,0x55..]
        custom = if customVMSColor rb then [0x1] else [0x0]
        blue = [blueVMS rb]
        green = [greenVMS rb]
        red = [greenVMS rb]
        alpha = [alphaComponent rb]
        padding = take 0x1B [0x0,0x0..]
        ts = exportTimestamp $ timeStamp rb
        padding2 = take 0x8 [0x0,0x0..]
        unknown1 = unknownValues1 rb
        fatLoc = splitW16Le $ locationFAT rb
        fatSize = splitW16Le $ sizeFAT rb
        dirLoc = splitW16Le $ locationDirectory rb
        dirSize = splitW16Le $ sizeDirectory rb
        is = splitW16Le $ iconShape rb
        ubc = splitW16Le $ userBlocksCount rb
        unknown2 =unknownValues2 rb

exportTimestamp :: Timestamp -> [Word8]
exportTimestamp ts = ct ++ yr ++ mnth ++ dy ++ hr ++ m ++ sec ++ dow
    where
        ct   = [century ts]
        yr   = [year ts]
        mnth = [month ts]
        dy   = [day ts]
        hr   = [hour ts]
        m    = [minute ts]
        sec  = [second ts]
        dow  = [dayOfWeek ts]


