{-

Link to our Github project:  https://github.com/Scanteianu/ParallelFunctionalProgrammingProject
Link to our Google Driver folder: https://docs.google.com/presentation/d/1bvrL5256hh0KythpZ8TOJ6gy8PVsm867huHGsoqHxdw/edit#slide=id.gb24068de60_0_39


Instructions for program∷
1) Install the package (only once)
$ stack install parallel 

2) git clone the project repo 
$ git clone https://github.com/Scanteianu/ParallelFunctionalProgrammingProject.git

2) Compile the program 
$ stack ghc -- -threaded --make -Wall -O Tfidf.hs -XDeriveAnyClass -XDeriveGeneric

3) Sample tests 
$ ./Tfidf +RTS -N1 -RTS -f "../SampleTestFiles/file1.txt" "I love cat dog bird" 4
0.000078s
[("I love cat dog bird",2.283333333333333)]

$ ./Tfidf +RTS -N4 -RTS -f "../SampleTestFiles/twitterLargeStrings.txt" "airline delay" 4
16.12025s
[(" checked in 2 minutes after my flight became eligible for check in and I\226\8364\8482m in boarding group C @SouthwestAir HOW https://t.co/Hc9fKCdUJK @",16.166666666666664)]

$ ./Tfidf +RTS -N4 -RTS -d "../SmallInputFiles" "author" 4
4.466509s
[("<!DOCTYPE html>\n<html class=\"client-nojs\" lang=\"en\" dir=\"ltr\">\n<head>\n<meta charset=\"UTF-8\"/>\n<title>George R. R. Martin - Wikipedia</title>",4.5)]

4) Run experiments 
4.a) Download 100ktwitter.txt, 500ktwitter.txt, 1mktwitter.txt from the Google Drive folder and move them to SampleTestFiles folder. These files are too large to push to github repo. 

4.b) comment out the "print $ results" and uncomment the  "-- putStr "\n" to get a better format of experiment results.

4.c) Recompile the program 

4.d) End-to-end test 
$ python3 runAndTime.py 
timing!
100k:
1,7.944977786805895,0.3144225196171566 
......

4.e) Only time the maxAnsSort step to test various thread effects (This is a parallel step.)
$ ./testThreads.sh | grep ".\|Time\|Thread"
Time: maxSort
Thread 1
6.819989s

4.d) Test different chunks and time the maxAnsSort step
$ ./testChunks.sh | grep ".\|Time\|Thread"
4 chunks
Time: maxSort
Thread 1
6.738853s
...

-}

import System.Environment(getArgs, getProgName)
import System.Directory as Dir
import DocumentIndexing
import System.Exit(exitFailure)
import System.FilePath ((</>))
import Data.Char
import Prelude
import Data.Time


main :: IO ()
main = do
   args <- getArgs
   errCheckStatus <- checkInputErrors args
   case errCheckStatus of
       1 -> do
        pn <- getProgName
        putStr $ "Usage: "++ pn ++" -f <filesname> <search-phase> <number-of-chunks> or  "++ pn ++" -d <files-path> <search-phase> <number-of-chunks>\n"
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

        results <- runTfidfNoPrint (read (args !! 3)::Int)  (head args) (args!!1) (args !! 2) 

        -- You can comment out the "print $ results" and uncomment the  "-- putStr "\n" to get a better format of experiment results
        print $ results
        -- putStr "\n"

runTfidfNoPrint :: Int -> String -> String -> String  -> IO [(String,Double)]
runTfidfNoPrint chunks flag files keys = do
    documents <- getAllDocuments flag files
    let allWords = getAllTheWords documents
    let globFreq = getGlobalDocumentFrequency documents allWords
    let indexDocs = updateDocumentsWithTfIdfScore documents globFreq
    t1 <- indexDocs `seq` getCurrentTime
    
    let result =  Prelude.take 5 (simplifyOutput (maxAndSort chunks keys indexDocs))
    t2 <- result `seq` getCurrentTime
    -- print $ "Time: maxSort"
    print $ diffUTCTime t2 t1

    return result 


-- A function that either reads from a single file to turn each line to a Document or reads from a directory to turn each file to be a Document 
getAllDocuments :: String -> String -> IO [Document]
getAllDocuments flag file
    | flag == "-d" = do 
        files <- getDirectoryFiles file
        readFilesToDocuments files
    | flag == "-f" = do
        fileText <- readFile file 
        let documents = singleThreadedReadDocuments $ lines fileText 
        return documents
    | otherwise = return []


-- A funtion to check if there is any input errors 
checkInputErrors :: [String] -> IO Int 
checkInputErrors args
    | length args /= 4 = return 1
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




