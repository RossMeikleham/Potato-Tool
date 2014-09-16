import VMU
import System.IO
import System.Environment
import GHC.IO.Handle.FD
import qualified Data.ByteString.Lazy as BS

main :: IO()
main = do 
        args <- getArgs
        progName <- getProgName

        let vmuFile = head args
             
        if length args <= 0 
            then putStrLn ("Usage " ++ progName ++ " [vmu file]")
            else do 
                    bs <- BS.readFile vmuFile 
                    case createVMU  bs of
                        Left str -> error str
                        Right vmu -> putStrLn $ show $ files vmu
