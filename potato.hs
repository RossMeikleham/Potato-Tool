import Data.Word
import Data.Binary
import Data.Maybe
import Data.Char
import System.IO
import System.Environment
import GHC.IO.Handle.FD
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Bits
import Control.Applicative


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

data RootBlock = RootBlock
    { customVMSColor :: Word8 -- Bool
    , blueVMS :: Word8
    , redVMS :: Word8
    , greenVMS :: Word8
    , alphaComponent :: Word8
  --  , timeStamp
    ,locationFAT :: Word16
    ,sizeFAT :: Word16
    ,locationDirectory :: Word16
    ,sizeDirectory :: Word16
    ,iconShape :: Word16
    ,userBlocks :: Word16
    } deriving Show

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

-- Obtain the last block Root Block so we can determine
-- information on the file system to operate on it
getRootBlock :: [Word8] -> RootBlock
getRootBlock fileStr = 
        RootBlock customColor blue red green alpha 
                  locationFAT sizeFAT locationDir sizeDir iconShape userBlocks
        
        where 
              rootBlockStr = drop (blockStart 255) fileStr
              customColor = rootBlockStr !! 0x10 -- /= 0x0
              blue = rootBlockStr !! 0x11
              green = rootBlockStr !! 0x12
              red = rootBlockStr !! 0x13
              alpha = rootBlockStr !! 0x14
              locationFAT = encodeWord16 $ slice 0x46 0x47 rootBlockStr
              sizeFAT = encodeWord16 $ slice 0x48 0x49 rootBlockStr
              locationDir = encodeWord16 $ slice 0x4A 0x4B rootBlockStr
              sizeDir = encodeWord16 $ slice 0x4C 0x4D rootBlockStr
              iconShape = encodeWord16 $ slice 0x4E 0x4F rootBlockStr
              userBlocks = encodeWord16 $ slice 0x50 0x51 rootBlockStr


-- Read 32 Bytes entry into a Directory Entry
-- If file type is none or unrecognized value is read in
-- then Nothing is returned as it is not a valid directory entry
-- TODO possibly distinguish between corrupt and no file
getDirEntry :: [Word8] -> Maybe DirectoryEntry
getDirEntry entry = 
    DirectoryEntry <$> fType <*> protected <*> startingB <*> 
        name <*> sizeB <*> offsetB 

    where 
        fType = case entry !! 0x0 of
                0x33 -> Just Game
                0xCC -> Just Data
                otherwise ->  Nothing
    
        protected = case entry !! 0x1 of
                0x00 -> Just False
                0xFF -> Just True
                otherwise -> Nothing

        startingB = Just $ encodeWord16 $ slice 0x2 0x3 entry
        name = Just $ map (chr . fromEnum) $ slice 0x4 0xF entry
        sizeB = Just $ encodeWord16 $ slice 0x18 0x19 entry
        offsetB = Just $ encodeWord16 $ slice 0x1A 0x1B entry

getDirectory :: RootBlock -> [Word8] -> Directory
getDirectory rb vmu = Directory $ catMaybes entries
    where dirBlockStart = fromIntegral $ locationDirectory rb
          noBlocks = fromIntegral $ sizeDirectory rb
          dirSizeBytes =  blockStart $ fromIntegral noBlocks

          entries = 
            concatMap (entriesBlock) [dirBlockStart,dirBlockStart-1..dirBlockStart - noBlocks]

          -- 16 32 bytes entries in 512 byte block
          entriesBlock n = [getDirEntry $ 
            slice ((blockStart n) + x * 32) ((blockStart n) - 1 + ((x + 1) * 32)) vmu | 
                x <- [0..15]] 


displayContents :: BS.ByteString -> String
displayContents bs = show rb  ++ (show $ getDirectory rb (BS.unpack bs))
    where rb = getRootBlock $ BS.unpack bs

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
                 hClose file
                 if size /= vmuSize
                     then putStrLn ("VMU is incorrect size (" ++ (show size) ++ " bytes)")
                     else do 
                            bs <- BS.readFile vmu 
                            putStrLn $ displayContents bs
                 
                 
                  
