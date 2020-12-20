

{-

New compilation/execution instructions for parallelism∷
1) stack install parallel (only once)

2) stack ghc -- -threaded --make -Wall -O Tfidf.hs -XDeriveAnyClass -XDeriveGeneric

import System.Directory as Dir
    ( doesDirectoryExist, doesFileExist, getDirectoryContents )
    ( doesDirectoryExist, doesFileExist, getDirectoryContents ) -f "../SampleTestFiles/twitterCustomerSupportTruncated.txt" "airline delay"

3a) ./Tfidf +RTS -N4 -RTS -f "../SampleTestFiles/twitterLargeStrings.txt" "airline delay"
3b) ./Tfidf +RTS -N4 -RTS -d "../SmallInputFiles" "author"

./test.sh | grep ".\|Time\|Thread"
-}

import System.Environment(getArgs, getProgName)
import System.Directory as Dir
import DocumentIndexing
import System.Exit(exitFailure)
import System.FilePath ((</>))
import Data.Char
import Data.Set as Set 
import Prelude
import Data.Time


main :: IO ()
main = do
   args <- getArgs
   errCheckStatus <- checkInputErrors args
   case errCheckStatus of
       1 -> do
        pn <- getProgName
        putStr $ "Usage: "++ pn ++" -f <filesname> <search-phase> or  "++ pn ++" -d <files-path> <search-phase>\n"
        exitFailure
       2 -> do
        putStr $ "Flag Options: -f (read from a file)or -d (read from a directory)"
        exitFailure
       3 -> do
        putStr $ "<search-phase> can only be letters.\n"
        exitFailure
       4 -> do
        putStr $ "Empty directory: " ++ args !! 1 ++ "\n"
        exitFailure
       5 -> do
        putStr $ "Should pass a vlid directory with -d flag.\n"
        exitFailure
       6 -> do
        putStr $ "Empty file: " ++ args !! 1 ++ "\n"
        exitFailure
       7 -> do
        putStr $ "Should pass a vlid file with -f flag.\n"
        exitFailure
       _ -> do

        results <- runTfidfNoPrint (head args) (args!!1) (args !! 2) 
        print $ results
        -- putStr "\n"

runTfidfNoPrint :: String -> String -> String -> IO [(String,Double)]
runTfidfNoPrint flag files keys = do
    documents <- getAllDocuments flag files
    let searchWordsSet =  Set.fromList $ Prelude.map sanitizeWord $ words $ keys
    let globFreq = getGlobalDocumentFrequency documents searchWordsSet
    let indexDocs = updateDocumentsWithTfIdfScore documents globFreq
    t1 <- indexDocs `seq` getCurrentTime
    
    let result =  Prelude.take 5 (simplifyOutput (maxAndSort keys indexDocs))
    t2 <- result `seq` getCurrentTime
    -- print $ "Time: maxSort\n"
    print $ diffUTCTime t2 t1

    return result 


-- A function that either reads from a single file to turn each line to a Document or reads from a directory to turn each file to be a Document 
getAllDocuments :: String -> String -> IO [Document]
getAllDocuments  flag file
    | flag == "-d" = do 
        files <- getDirectoryFiles file
        readFilesToDocuments files
    | flag == "-f" = do
        content <- readFile file
        return $ Prelude.map readDocument $ lines content 
    | otherwise = return []


-- A funtion to check if there is any input errors 
checkInputErrors :: [String] -> IO Int 
checkInputErrors args
    | length args /= 3 = return 1
    | not (head args == "-f" || head args == "-d")  = return 2
    | not (validateSearchPhase (args !! 2)) = return 3
    | otherwise = do
        if head args == "-d"
            then checkValidDir (args !! 1)
            else checkValidFile (args !! 1) 

checkValidDir :: String -> IO Int 
checkValidDir dir = do 
    isDir <- doesDirectoryExist dir 
    if isDir
        then do  
            files <- getDirectoryFiles dir 
            if length files == 0
                then return 4
                else return 0
        else return 5


checkValidFile :: String -> IO Int 
checkValidFile file = do 
    isFile <- doesFileExist file 
    if isFile 
        then do  
            content <- readFile file 
            if lines content == []
                then return 6 
                else return 0 
        else return 7 

validateSearchPhase :: [Char] -> Bool 
validateSearchPhase [] = True 
validateSearchPhase (x : xs)
    | isLetter x || isSpace x || x == ',' = validateSearchPhase xs 
    | otherwise = False 


-- A function to get a list of all fils in a directory 
getDirectoryFiles :: String -> IO [FilePath] 
getDirectoryFiles dir = do 
    files <- Prelude.filter (\x -> x /= "." && x /= "..") <$>  Dir.getDirectoryContents dir
    return $ Prelude.map (dir </>) files


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





