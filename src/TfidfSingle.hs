{-
    TFIDF project: A single thread

$ stack ghc -- --make -Wall -O TfidfSingle.hs
Usage: TfidfSingle -f <filesname> <search-phase> or  TfidfSingle -d <files-path> <search-phase>

"-d" flag reads each file of a directory into a Document.
"-f" flag reads each line of a file into a Document.

-- Test a simple file
$ ./TfidfSingle -f "../SampleTestFiles/file1.txt" "I love cat dog bird"
[[("I love cat dog bird" fromList [("bird",1),("cat",1),("dog",1),("i",1),("love",1)] fromList ["bird","cat","dog","i",,"love"] fromList [("bird",1.0),("cat",0.3333333333333333),("dog",0.5),("i",0.2),("love",0.25)],2.283333333333333)]
-}

{-

New compilation/execution instructions for parallelism∷
1) stack install parallel (only once)

2) stack ghc -- --make -Wall -O TfidfSingle.hs -XDeriveAnyClass -XDeriveGeneric

3) ./TfidfSingle -f "../SampleTestFiles/twitterCustomerSupportTruncated.txt" "airline delay"
-}

import System.Environment(getArgs, getProgName)
import System.Directory as Dir
import DocumentIndexing
import System.Exit(exitFailure)
import System.FilePath ((</>))
import Data.Char
import Data.Set as Set
import Prelude


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

        -- Get all the Documents
        docs <- getAllDocuments (head args) (args !! 1)

        -- Get all the search words
        let searchWordsSet =  Set.fromList $ Prelude.map sanitizeWord $ words $ args !! 2

        -- Update the tfidf
        let docsWithTfidf =  updateDocumentsWithTfIdfScore docs $ getGlobalDocumentFrequency docs searchWordsSet

        -- Sort the Documents with tfidf scores
        let sortedDocumentsWithScore = fmap simplifyOutput (searchAndSortSeq (args !! 2) docsWithTfidf)

        let results = fmap (Prelude.take 5) sortedDocumentsWithScore

        outputStr <- fmap show results
        print $ outputStr


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
