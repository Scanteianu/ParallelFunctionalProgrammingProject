{-
    TFIDF project: A single thread 

-- Might need them 
import System.Exit(exitFailure)
import Data.Char ( isLower )
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List


$ stack ghc -- --make -Wall -O TfidfSingle.hs 
$ ./TfidfSingle <files-path> <search-phase>

-}


import System.Environment(getArgs, getProgName)
import System.Directory
import DocumentIndexing
import System.Exit(exitFailure)
import System.FilePath ((</>))  

main :: IO ()
main = do 
   args <- getArgs 
   case args of 
       [dir, _] -> do
        files <- getDirectoryFiles dir 
        if length files == 0
            then putStr $ "Empty directory in " ++ dir ++ "\n"
            else do 
                docs <- readFilesToDocuments files 
                print docs 
       _ -> do 
        pn <- getProgName
        putStr $ "Usage: "++pn++" <files-path> <search-phase>\n"
        exitFailure


-- A function to get a list of all fils in a directory 
getDirectoryFiles :: String -> IO [FilePath] 
getDirectoryFiles dir = do 
    files <- filter (\x -> x /= "." && x /= "..") <$>  getDirectoryContents dir
    return $ map (dir </>) files

-- A function to read all files into Documents 
readFilesToDocuments ::[FilePath] -> IO [Document]
readFilesToDocuments files = sequence $ helperFunc files 
    where 
        helperFunc [] = []
        helperFunc (f : fs) = readFileToString f : helperFunc fs
            where
                readFileToString filePath = do 
                    fileContent <- readFile filePath
                    return $ readDocument fileContent  




