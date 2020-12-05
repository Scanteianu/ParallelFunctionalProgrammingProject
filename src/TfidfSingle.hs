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
import Data.Char

main :: IO ()
main = do 
   args <- getArgs
   errCheckStatus <- checkInputErrors args 
   case  errCheckStatus of 
       1 -> do
        pn <- getProgName
        putStr $ "Usage: "++ pn ++" <files-path> <search-phase>\n"
        exitFailure
       2 -> do
        putStr $ "<search-phase> can only be letters.\n"
        exitFailure
       3 -> do
        putStr $ "Empty directory: " ++ head args ++ "\n"
        exitFailure
       _ -> do
        files <- getDirectoryFiles $ head args

        -- Get all the Documents
        docs <- readFilesToDocuments files 

        -- Get all the search words 
        let ws =  map sanitizeWord $ words $ args !! 1  

        print ws 


        


checkInputErrors :: [String] -> IO Int 
checkInputErrors args
    | length args /= 2 = return 1
    | not (validateSearchPhase (args !! 1)) = return 2
    | otherwise = do 
        files <- getDirectoryFiles (head args)
        if null files
            then return 3
            else return 0


validateSearchPhase :: [Char] -> Bool 
validateSearchPhase [] = True 
validateSearchPhase (x : xs)
    | isLetter x || isSpace x || x == ',' = validateSearchPhase xs 
    | otherwise = False 

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







