

{-

Link to our Github project:  https://github.com/Scanteianu/ParallelFunctionalProgrammingProject
Link to our Google Driver folder: https://docs.google.com/presentation/d/1bvrL5256hh0KythpZ8TOJ6gy8PVsm867huHGsoqHxdw/edit#slide=id.gb24068de60_0_39


New compilation/execution instructions for parallelism∷
1) Install the package (only once)
$ stack install parallel 

2) git clone the project repo 
$ git clone https://github.com/Scanteianu/ParallelFunctionalProgrammingProject.git

2) Compile the program 
$ stack ghc -- -threaded --make -Wall -O Tfidf.hs -XDeriveAnyClass -XDeriveGeneric

3) Sample tests 
$ ./Tfidf +RTS -N1 -RTS -f "../SampleTestFiles/file1.txt" "I love cat dog bird"
0.000089s
[("I love cat dog bird",2.283333333333333),("I love cat dog",1.2833333333333332),("I love cat",0.7833333333333333),("I love",0.45),("I ",0.2)]

$ ./Tfidf +RTS -N4 -RTS -f "../SampleTestFiles/twitterLargeStrings.txt" "airline delay"
16.384462s
[(" checked in 2 minutes after my flight became eligible for check in and I\226\8364\8482m in boarding group C @SouthwestAir HOW https://t.co/Hc9fKCdUJK @",16.166666666666664),(" @Delta Our plane just crashed into the world trade center @Delta I have a problem with the flight I'm on right now it's urgent please respo",12.666666666666666),(" @hulu_support On live tv DVR, ~63 minutes. At Hulu online 85 via browser w/ no ads. This is first week with DVR ads and they are no-skip ad",11.083333333333332),(" @136460 Hello Kelsie, we hate to hear you having issues with our customer service. But how can I help you today? @sprintcare -im sure if u ",10.916666666666668),(" @116245 @ArgosHelpers She said sorry for the delay then buggered off. No thankyou or there's your items, just waltzed off \240\376\732\171 I was stood ",9.416666666666666)]

$ ./Tfidf +RTS -N4 -RTS -d "../SmallInputFiles" "author"
2.859438s
[("<!DOCTYPE html>\n<html class=\"client-nojs\" lang=\"en\" dir=\"ltr\">\n<head>\n<meta charset=\"UTF-8\"/>\n<title>George R. R. Martin - Wikipedia</title>",4.5),("<!DOCTYPE html>\n<html class=\"client-nojs\" lang=\"en\" dir=\"ltr\">\n<head>\n<meta charset=\"UTF-8\"/>\n<title>A Song of Ice and Fire - Wikipedia</tit",3.25),("\n\n\n\nGame of Thrones - Wikipedia\ndocument.documentElement.className=\"client-js\";RLCONF={\"wgBreakFrames\":!1,\"wgSeparatorTransformTable\":[\"\",\"\"",1.5),("<!DOCTYPE html>\n<html class=\"client-nojs\" lang=\"en\" dir=\"ltr\">\n<head>\n<meta charset=\"UTF-8\"/>\n<title>A Dance with Dragons - Wikipedia</title",1.25),("<!DOCTYPE html>\n<html class=\"client-nojs\" lang=\"en\" dir=\"ltr\">\n<head>\n<meta charset=\"UTF-8\"/>\n<title>Winter Is Coming - Wikipedia</title>\n<s",0.0)]


4) Run the experiment with a simple shell program 
4.a) Download 100ktwitter.txt, 500ktwitter.txt, 1mktwitter.txt from the Google Drive folder and move them to SampleTestFiles folder. These files are too large to push to github repo. 
4.b) ./test.sh | grep ".\|Time\|Thread"
4.c) Note the above experiment only captures the maxAndSort parallel part. 


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
    let allWords = getAllTheWords documents
    let globFreq = getGlobalDocumentFrequency documents allWords
    let indexDocs = updateDocumentsWithTfIdfScore documents globFreq
    t1 <- indexDocs `seq` getCurrentTime
    
    let result =  Prelude.take 5 (simplifyOutput (maxAndSort keys indexDocs))
    t2 <- result `seq` getCurrentTime
    print $ "Time: maxSort"
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





